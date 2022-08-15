      *****************************************************************
     h copyright('(C) Copyright 1996 - 2011 King III Solutions, Inc.  +
     h Rel 5.06 2011-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') FIXNBR(*ZONED) AUT(*ALL)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2011 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_9015
      **   Type: ILE RPG Program
      **   Desc: Update K_INTPROD via K_PRODLNK file
      **
      *****************************************************************
      **
      **  This program is used to see if products are linked, and if
      **  so, perform the proper combining function.
      **
      **  It will look for 'D' Daily type records only, by-pass any
      **  other types of records.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/19/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_PRODLNK.  Also, changed it to
      *            use SQL select statements to access the proper
      *            K_INTPROD records and an SQL update statement
      *            to update the correct K_INTPROD record.
      *****************************************************************
      * -------------------------------------------------- Work fields
     d dlysale         s                   like(ip_dlysale)
     d dlyouts         s                   like(ip_dlyouts)
     d qtyohnd         s                   like(ip_qtyohnd)
     d qtyoord         s                   like(ip_qtyoord)
     d qtyback         s                   like(ip_qtyback)
     d not_used        s               d   inz(d'0001-01-01') datfmt(*iso)
     d cmsysdate       s               d

     d pkcomp          s                   like(pk_comp)
     d pkfrmlocn       s                   like(pk_frmlocn)
     d pkfrmsupl       s                   like(pk_frmsupl)
     d pkfrmsub        s                   like(pk_frmsub)
     d pkfrmprod       s                   like(pk_frmprod)
     d pktolocn        s                   like(pk_to_locn)
     d pktosupl        s                   like(pk_to_supl)
     d pktosub         s                   like(pk_to_sub)
     d pktoprod        s                   like(pk_to_prod)
      * ------------------------------------------------------parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_9015        PI
     d  comp                          1
      * --------------------------------------------------------------------
     d prodlnk_rec   e ds                  ExtName(k_prodlnk)
     d intprod_rec   e ds                  ExtName(k_intprod)
      * --------------------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exec sql
         select cm_sysdate
           into :cmsysdate
           from k_company
           where cm_comp = :comp;

       exsr dclpkcursor;
       //exsr clspkcursor;
       exsr opnpkcursor;


       // loop through all product link records
       dow SQLSTT = SQLStateOk;

         exec sql
           fetch next
           from pkcursor
           into :prodlnk_rec;

         if SQLState = RowNotFound;
           leave;
         endif;

       // process 'D' Daily records only
         if pk_comp = comp AND pk_type = 'D' AND
            (pk_dislink = not_used    OR
             pk_dislink > cmsysdate);

       // get information out of the from location
       // products interface file
             exec sql
              select *
                into :intprod_rec
                from k_intprod
                where ip_comp = :pk_comp and
                      ip_locn = :pk_frmlocn and
                      ip_supl = :pk_frmsupl and
                      ip_suplsub = :pk_frmsub and
                      ip_prod = :pk_frmprod
                fetch first row only;

             if SQLState = SQLStateOk;

                pkcomp = pk_comp;
                pkfrmlocn = pk_frmlocn;
                pkfrmsupl = pk_frmsupl;
                pkfrmsub = pk_frmsub;
                pkfrmprod = pk_frmprod;

                dlysale = ip_dlysale;
                dlyouts = ip_dlyouts;
                qtyohnd = ip_qtyohnd;
                qtyoord = ip_qtyoord;
                qtyback = ip_qtyback;

        //update the to location
                exec sql
                select *
                  into :intprod_rec
                  from k_intprod
                  where ip_comp = :pk_comp and
                        ip_locn = :pk_to_locn and
                        ip_supl = :pk_to_supl and
                        ip_suplsub = :pk_to_sub and
                        ip_prod = :pk_to_prod
                  fetch first row only;
                if SQLState = SQLStateOk;

                   pkcomp = pk_comp;
                   pktolocn = pk_to_locn;
                   pktosupl = pk_to_supl;
                   pktosub = pk_to_sub;
                   pktoprod = pk_to_prod;

                   ip_dlysale += dlysale;
                   ip_dlyouts += dlyouts;
                   ip_qtyohnd += qtyohnd;
                   ip_qtyoord += qtyoord;
                   ip_qtyback += qtyback;

                  exsr updtintprod;
                else;
                  if SQLState = RowNotFound;
                     SQLState = SQLStateOk;
                  endif;
                endif;
             else;
                if SQLState = RowNotFound;
                   SQLState = SQLStateOk;
                endif;
             endif;
         endif;
       enddo;

       exsr clspkcursor;

       *inlr = *on;

       begsr dclpkcursor;
       exec sql
        declare pkcursor Cursor
          for
        select *
          from k_prodlnk;
       endsr;

       begsr opnpkcursor;
       exec sql
        open pkcursor;
       endsr;

       begsr clspkcursor;
       exec sql
        close pkcursor;
       endsr;

       begsr updtintprod;
       exec sql
         update k_intprod
           set ip_dlysale = :ip_dlysale,
               ip_dlyouts = :ip_dlyouts,
               ip_qtyohnd = :ip_qtyohnd,
               ip_qtyoord = :ip_qtyoord,
               ip_qtyback = :ip_qtyback
           where ip_comp = :pk_comp and
                 ip_locn = :pk_to_locn and
                 ip_supl = :pk_to_supl and
                 ip_suplsub = :pk_to_sub and
                 ip_prod = :pk_to_prod;
       endsr;
      /end-free
