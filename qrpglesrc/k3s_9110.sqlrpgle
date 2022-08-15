      *****************************************************************
      *
     h copyright('(C) Copyright 1996 - 1999 King III Solutions, Inc.  +
     h Rel 4.19 1999-03-30       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5')
     h AUT(*ALL) OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-1999 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_9110
      **   Type: ILE RPG Program
      **   Desc: Build products records for alternate source entries
      **
      *****************************************************************
      **
      **  This program will read the alternate source entries and
      **  generate product records
      **
      *****************************************************************
      **
      **  06/30/2005 - This section of logic helps to display buy group
      **               in PR_DESC1, and to set sequence by buy group
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/04/2014.
      **  Remarks. Altered program to utilize an SQL cursor to loop
      **           through K_INTALTR records and write to report.
      **           Also, utilized an SQL select statement to access
      **           desired K_SUPLIER record. Finally, used an SQL
      **           select statement to verify product record exists
      **           for original supplier and another select statement
      **           to check if a K_PRODUCT record exists for
      **           alternate supplier. If it does not exist, used
      **           an SQL insert statement to add the record.
      *****************************************************************
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_9110        PI
     d  comp                          1
     d  rec_count                     9  0
      * ----------------------------------------- Supplier for Control Break
     d                 ds
     d supl                          25
     d  xx_locn                       5    overlay(supl:1)
     d  xx_supl                      10    overlay(supl:6)
     d  xx_suplsub                   10    overlay(supl:16)
     d suplsaved       s                   like(supl)

      * ---------------------------------------------------------
     d time_stamp      s               z   inz                                  time stamp
     d buyr            s              8a   inz                                  time stamp
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------------- Workfields
     d intaltr_rec   e ds                  ExtName(k_intaltr)
     d suplier_rec   e ds                  ExtName(k_suplier)
     d product_rec   e ds                  ExtName(k_product)

      /free
       //------------------------------------------------- get supplier info
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;
       //
       exsr dcliacursor;
       //exsr clsiacursor;
       exsr opniacursor;

       dow SQLSTT = SQLStateOk;

          exec sql
            fetch next
              from iacursor
              into :intaltr_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

          xx_locn = ia_locn;
          xx_supl = ia_supl;
          xx_suplsub = ia_suplsub;
       //change in supplier ID
          if supl <> suplsaved;
       //save new supplier id
             suplsaved = supl;

       //get original supplier order cycle, which will be passed via
       //    field pr_daysunt to field pq_orcycle in program k3s_1500
       //    Program k3s_1040 can then display regular source order cycle
       //    which is used for replenishment calculations
       //get supplier record
             exec sql
               select *
                 into :suplier_rec
                 from k_suplier
                 where sp_comp = :ia_comp and
                       sp_locn = :ia_locn and
                       sp_supl = :ia_supl and
                       sp_suplsub = :ia_suplsub
                 fetch first row only;

             if SQLState = RowNotFound;
       //if suplier record not found, default order cycle to 7 days
               sp_orcycle = 7;
             endif;

       //end supplier break
          endif;

       //-------------------------------------------- create product records
       //get original product

          exec sql
            select *
              into :product_rec
              from k_product
              where pr_comp = :ia_comp and
                    pr_locn = :ia_locn and
                    pr_supl = :ia_supl and
                    pr_suplsub = :ia_suplsub and
                    pr_prod = :ia_prod
              fetch first row only;

          if SQLState = SQLStateOk;

       //chain for add of alternate source product
             exec sql
               select *
                 into :product_rec
                 from k_product
                 where pr_comp = :ia_comp and
                       pr_locn = :ia_locn and
                       pr_supl = :ia_suplalt and
                       pr_suplsub = ' ' and
                       pr_prod = :ia_prod
                 fetch first row only;

             if SQLState = RowNotFound;

                   pr_comp = ia_comp;
                   pr_locn = ia_locn;
                   pr_supl = ia_suplalt;
                   pr_suplsub = *blanks;
                   pr_prod = ia_prod;

                   evalr buyr = pr_buyr;
                   pr_desc1 = %replace(buyr:pr_desc1:33:%len(buyr));
                   pr_prodseq = pr_buyr + ' ' + pr_prodseq;

                   pr_birth = %date(%subst(%char(time_stamp):1:10):*ISO);
                   pr_lastupd = %date(%subst(%char(time_stamp):1:10):*ISO);
                   pr_buyr    = ia_buyralt;
                   pr_altsrce = 1;

                   pr_disothr = pr_costeac - ia_costeac;
                   if pr_disothr < 0;
                      pr_disothr = 0;
                   endif;

                   pr_costreg = ia_cost;
                   pr_costdiv = ia_costdiv;
                   pr_costeac = ia_costeac;

       //save original supplier order cycle, to be passed on to pq_orcycle
       //     in k3s_1500 program
                   pr_daysunt = sp_orcycle;

       //compare supplier lead time to product lead time
                   if pr_leadtm < sp_leadtmo;
                      pr_leadtm = sp_leadtmo;
                   endif;

       //save original buy multiple into field pr_buymulo
                   pr_buymulo = pr_buymult;

       //include alternate source on order for total on order value
                   pr_qtyoord += pr_altoord;

       //special rounding rules from /copy member k3s_c230
       //*               exsr      $_spec_rnd

                   exsr insert_product;
                   rec_count += 1;
             endif;
          else;
            if SQLState = RowNotFound;
               SQLState = SQLStateOk;
            endif;
          endif;
       enddo;

       exsr clsiacursor;

       *inlr = *on;

       //////////////////////////////////////////////////////////// One time

       begsr *inzsr;

       //----------------------------------------------------- get timestamp
       //call subprocedure to retrieve time stamp
       callp K3S_Retrieve_Timestamp(time_stamp);

       clear rec_count;

       endsr;

       begsr dcliacursor;
       exec sql
        declare iacursor Cursor
          for
        select *
          from k_intaltr
          where ia_comp = :comp;
       endsr;

       begsr opniacursor;
       exec sql
        open iacursor;
        if SQLState <> SQLStateOk;
           exsr clsiacursor;
           exec sql
            open iacursor;
        endif;
       endsr;

       begsr clsiacursor;
       exec sql
        close iacursor;
       endsr;

       begsr insert_product;
         Exec sql
         insert into k_product
         values (:product_rec);
       endsr;
      /end-free
      *----------------------------------- special rounding alternate source
     c*copy k3s_c230
      *****************************************************************
      **
      **   K3S-Replenish - Inventory REPLENISHment System
      **   Copyright (c) 1996-1997 by King III Solutions, Inc.
      **   All rights reserved.
      **
      *****************************************************************
      **
      **   Name: K3S_C230
      **   Type: ILE /COPY member
      **   Desc: Special rounding used for alternate source products
      **
      *****************************************************************
      **
      **   This member is called out of program K3S_9110
      **
      *****************************************************************

      * ////////////////////////////////// special rounding alternate source

     c***  $_spec_rnd    begsr

     c***                if        pr_packsiz = '        50' or
     c***                          pr_packsiz = '        60' or
     c***                          pr_packsiz = '       100' or
     c***                          pr_packsiz = '     00100' or
     c***                          pr_packsiz = '       1OZ' or
     c***                          pr_packsiz = '       2OZ' or
     c***                          pr_packsiz = '       4OZ' or
     c***                          pr_packsiz = '       8OZ' or
     c***                          pr_packsiz = '      16OZ'
     c***                eval      pr_buymult= 12
     c***                else

     c***                eval      pr_buymult= 3
     c***                endif

     c***                endsr

