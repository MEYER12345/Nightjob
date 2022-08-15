      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2008 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_3011
      **   Type: ILE RPG Program
      **   Desc: Product copy history in batch
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/20/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_PRODLNK.  Also, changed it to
      *            use an SQL update statement to update fields
      *            PK_COPYCMP and PK_TRANSTS if a record has been
      *            copied. In addition, changed program to utilize
      *            SQL select statements to access K_PRODUCT and
      *            K_LOCATNS records.
      *****************************************************************
      * ---------------------------------- Message logic for screen programs
     d/copy k3s_c050
      * ----------------------------------- D-specs for common workfields
     d/copy k3s_c270
      * ----------------------------------------------------------------
     d/copy k3s_proto
      * ----------------------------------------------------------
     d prodlnk_rec   e ds                  ExtName(k_prodlnk)
     d product_rec   e ds                  ExtName(k_product)
      * ----------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * ----------------------------------------------------------
     d histype         s              1  0
     d program2        s                   like(program)
     d setfore         s                   like(pr_comp)
     d updtype         s                   like(pr_comp)
     d lcsysdate       s               d
      * ---------------------------------------------------------- Begin
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclpkcursor;
       //exsr clspkcursor;
       exsr opnpkcursor;

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
        dow SQLSTT = SQLStateOk;

        //fetch schedule record to be used for next system date
          exec sql
           fetch next
            from pkcursor
            into :prodlnk_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

          if pk_transts = 0 and
             pk_type = 'C';
               soqseq# = 001;
               setfore = %editc(pk_frmclr:'X');        //numeric to character
               if pk_frmlocn <> *blanks;

                 exec sql
                   select *
                     into :product_rec
                     from k_product
                     where pr_comp = :pk_comp and
                           pr_locn = :pk_frmlocn and
                           pr_supl = :pk_frmsupl and
                           pr_suplsub = :pk_frmsub and
                           pr_prod = :pk_frmprod
                     fetch first row only;

                 exec sql
                   select lc_sysdate
                     into :lcsysdate
                     from k_locatns
                     where lc_comp = :pk_comp and
                           lc_locn = :pk_frmlocn
                     fetch first row only;
               endif;

               if SQLState = SQLStateOk;
                  @updated = 0;
                  @returned = 0;
                  mode = *blanks;
                  prodseq = *blanks;
                  program = *blanks;
                  program2 = *blanks;
                  checktype = *blanks;
                  rollupdown = *blanks;
                  mode1 = *blanks;
                  histype = 0;
                  updtype = 'B';
                  callp K3S_3010(pk_comp:
                                 pr_buyr:
                                 pk_frmlocn:
                                 pk_frmsupl:
                                 pk_frmsub:
                                 pk_frmprod:
                                 @updated:
                                 @returned:
                                 mode:
                                 soqseq#:
                                 prodseq:
                                 program:
                                 program2:
                                 checktype:
                                 pk_frmsupl:
                                 pk_frmsub:
                                 rollupdown:
                                 mode1:
                                 histype:
                                 updtype:
                                 pk_to_locn:
                                 pk_to_supl:
                                 pk_to_sub:
                                 pk_to_prod:
                                 pk_chgtype:
                                 pk_frmstat:
                                 pk_frmfrez:
                                 setfore:
                                 pk_hstmult);
                  if @updated = 1;
                     if pk_frmlocn <> *blanks;
                        pk_copycmp = lcsysdate;
                     else;
                        pk_copycmp = %date();
                     endif;
                     pk_transts = 1;
                     exsr updtprodlnk;
                  endif;
               else;
                 SQLState = SQLStateOk;
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
         from k_prodlnk
         order by pk_comp,
                  pk_type,
                  pk_frmlocn,
                  pk_frmsupl,
                  pk_frmsub,
                  pk_frmprod,
                  pk_to_locn,
                  pk_to_supl,
                  pk_to_sub,
                  pk_to_prod
         for update of pk_copycmp,
                       pk_transts;
       endsr;

       begsr updtprodlnk;
       exec sql
        update k_prodlnk
          set pk_copycmp = :pk_copycmp,
              pk_transts = :pk_transts
          where current of pkcursor;
       endsr;

       begsr opnpkcursor;
       exec sql
        open pkcursor;
        if SQLState <> SQLStateOk;
           exsr clspkcursor;
           exec sql
            open pkcursor;
        endif;
       endsr;

       begsr clspkcursor;
       exec sql
        close pkcursor;
       endsr;

      /end-free
