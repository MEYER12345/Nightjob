      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2016 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_3920
      **   Type: ILE RPG Program
      **   Desc: Convert external source to k_intprod format
      **
      *****************************************************************
      **
      **  This program is used to accumulate daily product information
      **  from the K_INTPROD file into K_DLYPROD file.
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/08/2014.
      **  Remarks. Altered program to utilize SQL select statements to
      **           access K_ COMPANY, K_TABLCOD and K_PRODUCT records.
      **           Also, added an SQL cursor to loop through file
      **           K_INTPROD and an SQL insert statement to write
      **           records to file K_DLYPROD.
      *****************************************************************
     d time_stamp      s               z   inz
     d time20          s             20  0
     d time9           s              9  0
     d time3           s              3  0
     d once            s              1
     d daynbr          s              1p 0                                      # valid locations
     d weeks           s              7  0                                      # valid locations
     d saledatiso      s               d   datfmt(*iso)                         batch end date
     d demd_only       s              1p 0                                      capture demand only
     d cmsysdate       s               d
     d collect         s              1

     d                 ds
     dshift                    1      7  0
     dmonsub                   1      1  0                                      user ID
     dtuesub                   2      2  0                                      user ID
     dwedsub                   3      3  0                                      user ID
     dthusub                   4      4  0                                      user ID
     dfrisub                   5      5  0                                      user ID
     dsatsub                   6      6  0                                      user ID
     dsunsub                   7      7  0                                      user ID

      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d tablcod_rec   e ds                  ExtName(k_tablcod)
     d product_rec   e ds                  ExtName(k_product)
     d dlyprod_rec   e ds                  ExtName(k_dlyprod)
     d intprod_rec   e ds                  ExtName(k_intprod)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3920        PI
     d  comp                          1
     d  perend12                      1
     d  perend13                      1
     d  perend52                      1
     d*
      /free
       exec sql
       set option commit = *none,
                  datfmt = *iso,
                  closqlcsr = *endactgrp;

       // get data from table code file
       exec sql
         select *
           into :tablcod_rec
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'K3S_3920  COLLECT   '
         fetch first row only;
       if SQLState = SQLStateOk and ta_flag1 = 1;
          demd_only = ta_flag2;
          collect   = *on;

          exsr dclipcursor;
          //exsr clsipcursor;
          exsr opnipcursor;

          Dow SQLState = SQLStateok;
       //read products interface file record
             exec sql
               fetch next
                 from ipcursor
                 into :intprod_rec;
       //--------------------------------------------------------- Main Loop
             If SQLState = RowNotFound;
                leave;
             endif;

       // get data from product file
             exec sql
               select *
                 into :product_rec
                 from k_product
                 where pr_comp = :ip_comp and
                       pr_locn = :ip_locn and
                       pr_suplusr = :ip_suplusr and
                       pr_suplusb = :ip_suplusb and
                       pr_prod = :ip_prod
                 fetch first row only;
             if SQLState = SQLStateOk;

       // collect section for daily replenishment - determine shift
                if collect = *on;
                   exsr $_collect;
                endif;

                r1_locn    = ip_locn;
                r1_comp    = pr_comp;
                r1_supl    = pr_supl;
                r1_suplsub = pr_suplsub;
                r1_suplusr = ip_suplusr;
                r1_suplusb = ip_suplusb;
                r1_prod    = ip_prod;
                r1_usrstat = pr_usrstat;
                r1_sysstat = pr_sysstat;
                r1_costeac = pr_costeac;
                r1_sales   = ip_sales;
                r1_qtyohnd = ip_qtyohnd;
                r1_qtyoord = ip_qtyoord;
                r1_qtyback = ip_qtyback;
                r1_qtybaln = pr_qtybaln;
                r1_dlysale = ip_dlysale;
                r1_dlyouts = ip_dlyouts;
                r1_mfgout  = ip_mfgout;
                r1_forcint = pr_forcint;
                r1_okflag  = 0;
                r1_saledat = saledatiso;
                if pr_forcint = 12;
                   if perend12 = '0';
                      r1_forcyr = pr_forcyr;
                      r1_forcper = pr_forcper;
                   else;
                      r1_forcper = pr_forcper - 1;
                      if r1_forcper = 0;
                         r1_forcper = 12;
                         r1_forcyr = pr_forcyr - 1;
                      else;
                         r1_forcyr = pr_forcyr;
                         r1_forcper = pr_forcper - 1;
                      endif;
                   endif;
                endif;

                if pr_forcint = 13;
                   if perend13 = '0';
                      r1_forcyr = pr_forcyr;
                      r1_forcper = pr_forcper;
                   else;
                      r1_forcper = pr_forcper - 1;
                      if r1_forcper = 0;
                         r1_forcper = 13;
                         r1_forcyr = pr_forcyr - 1;
                      else;
                         r1_forcyr = pr_forcyr;
                         r1_forcper = pr_forcper - 1;
                      endif;
                   endif;
                endif;

                if pr_forcint = 52;
                   if perend52 = '0';
                      r1_forcyr = pr_forcyr;
                      r1_forcper = pr_forcper;
                   else;
                      r1_forcper = pr_forcper - 1;
                      if r1_forcper = 0;
                         r1_forcper = 52;
                         r1_forcyr = pr_forcyr - 1;
                      else;
                         r1_forcyr = pr_forcyr;
                         r1_forcper = pr_forcper - 1;
                      endif;
                   endif;
                endif;

       //for TRACS environment, only capture when demand exists
                if demd_only = 1;
                   if ip_dlysale > 0 OR
                      ip_dlyouts > 0;
       //write products daily information capture record
                         exsr insert_dlyprod;
                   endif;
                else;
       //write products daily information capture record
                   exsr insert_dlyprod;
                endif;
             endif;
             SQLState = SQLStateOk;
          enddo;
       endif;
       exsr clsipcursor;
       *inlr = *on;

       ////////////////////////////////////////////////////////// Initialize

       begsr *inzsr;

       //  set company company code
       exec sql
         select cm_sysdate
           into :cmsysdate
           from k_company
           where cm_comp = :comp
           fetch first row only;

       // get company date for record birth date
       r1_birth   = cmsysdate;

       //call module to retrieve timestamp
       callp K3S_Retrieve_Timestamp(time_stamp);
       time20 = %dec(%char(time_stamp:*iso0):20:0);

       //take a portion of time value for unique batch # on this day
       time9 =  %dec(%subst(%char(time20):12:9):9:0);
       time3 =  %dec(%subst(%char(time9):1:3):3:0);
       r1_batch#  = time3;

       endsr;

       begsr dclipcursor;
       exec sql
        declare ipcursor Cursor
          for
        select *
          from k_intprod
          where ip_comp = :comp;
       endsr;

       begsr opnipcursor;
       exec sql
        open ipcursor;
        if SQLState <> SQLStateOk;
           exsr clsipcursor;
           exec sql
            open ipcursor;
        endif;
       endsr;

       begsr clsipcursor;
       exec sql
        close ipcursor;
       endsr;

       begsr insert_dlyprod;
         Exec sql
           insert into k_dlyprod
           values (:dlyprod_rec);
       endsr;

       ///////////////////////////////////////////////////////// collect

       begsr $_collect;

       exec sql
         select *
           into :tablcod_rec
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'K3S_3920  SHIFT'||:ip_locn
         fetch first row only;
       if SQLState = SQLStateOk and ta_flag1 = 1;
          shift = %dec(%subst(ta_codeds3:1:7):7:0);
       else;
          exec sql
            select *
              into :tablcod_rec
              from k_tablcod
              where ta_comp = :comp and
              ta_codetyp = 'APP' and
              ta_codeval = 'K3S_3920  SHIFT     '
          fetch first row only;
          if SQLState = SQLStateOk and ta_flag1 = 1;
             shift = %dec(%subst(ta_codeds3:1:7):7:0);
          else;
             shift = %dec(%subst('0000000':1:7):7:0);
          endif;
       endif;

       //  find sunday
       weeks = %diff(r1_birth:d'1900-12-30':*days);
       daynbr = %rem(weeks:7);
       saledatiso = r1_birth;

       select;
         when daynbr = 0;
           saledatiso = saledatiso - %days(sunsub);
         when daynbr = 1;
           saledatiso = saledatiso - %days(monsub);
         when daynbr = 2;
           saledatiso = saledatiso - %days(tuesub);
         when daynbr = 3;
           saledatiso = saledatiso - %days(wedsub);
         when daynbr = 4;
           saledatiso = saledatiso - %days(thusub);
         when daynbr = 5;
           saledatiso = saledatiso - %days(frisub);
         when daynbr = 6;
           saledatiso = saledatiso - %days(satsub);
         other;
       endsl;

       endsr;
      /end-free
