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
      **   Name: K3S_8540
      **   Type: ILE RPG Program
      **   Desc: Product copy history for transfer supplier (batch)
      **
      *****************************************************************
      **
      **  This program is used to copy product history
      **
      *****************************************************************

     fk_prodhisbuf a e           k disk
      * products histopry

     fk_prodh52buf a e           k disk
      * products histopry

      * ------------------------------------ File information data structure
     d/copy k3s_c010

      * ------------------------------------------- Function key definitions
     d/copy k3s_c020

      * ---------------------------------------------------- Local Data Area
     d/copy k3s_c030

      * -------------------------------------- Program Status Data Structure
     d/copy k3s_c040

      * ----------------------------------- D-specs for common workfields
     d/copy k3s_c270

      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_8540        PI
     d  wcomp                         1
     d  zz_frlocn                     5
     d  zz_frsupl                    10
     d  zz_frsub                     10
     d  zz_frprod                    25
     d  zz_tolocn                     5
     d  zz_tosupl                    10
     d  zz_tosub                     10
     d  zz_toprod                    25
     d  zz_multply                    9  4
     d  zz_histype                    1  0
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -----------------------------------------------------
     d product_rec   e ds                  ExtName(k_product)
     d locatns_rec   e ds                  ExtName(k_locatns)
      * ------------------------------------------------------
     d*accdem          s              7  0 dim(52)
     d xx_tosub        s                   like(pr_suplsub)
     d xx_frsupl       s                   like(pr_supl)
     d xx_frsub        s                   like(pr_suplsub)
     d supla           s                   like(pr_supl)
     d suplsuba        s                   like(pr_suplsub)
     d xx_year         s                   like(ph_year)
     d blank           s                   like(pr_comp)
     d histype         s                   like(ph_histype)
     d sz_histype      s                   like(ph_histype)
     d sz_frlocn       s                   like(pr_locn)
     d zz_forcast      s                   like(pr_forcast)
     d zz_forserr      s                   like(pr_forserr)
     d zz_seasonl      s                   like(pr_seasonl)
     d zz_frdelt       s                   like(pr_deltcnt)
     d zz_todelt       s                   like(pr_deltcnt)
     d zz_accsale      s                   like(pr_accsale)
     d zz_accouts      s                   like(pr_accouts)
     d zz_accdem       s                   like(pr_accdem)
     d zz_usrstat      s                   like(pr_usrstat)
     d sz_forcast      s                   like(pr_forcast)
     d sz_fordevp      s                   like(pr_fordevp)
     d zz_fordevp      s                   like(pr_fordevp)
     d zz_frforci      s                   like(pr_forcint)
     d zz_frbuyr       s                   like(pr_buyr)
     d zz_fstslpr      s                   like(pr_fstslpr)
     d zz_fstslyr      s                   like(pr_fstslyr)
     d sz_frsupl       s                   like(pr_supl)
     d sz_frsub        s                   like(pr_suplsub)
     d sz_frprod       s                   like(pr_prod)
     d sz_tolocn       s                   like(pr_locn)
     d zz_toforci      s                   like(pr_forcint)
     d zz_tobuyr       s                   like(pr_buyr)
     d sz_tosupl       s                   like(pr_supl)
     d sz_tosub        s                   like(pr_suplsub)
     d sz_toprod       s                   like(pr_prod)

     d add             s              1
     d replace         s              1
     d domany          s              1
     d ck_userupd      s              1  0
     d @_normal1       s              1a
     d save_rrn        s              5  0
     d pf_chgd1        s              7
     d pf_chgd2        s             13
     d day_differ      s              3  0
     d xx_forcper      s              3  0
     d componce        s              5  0
     d program1        s                   like(program)
     d program2        s                   like(program)
     d program3        s                   like(program)
     d pgm             s                   like(program)

     d per_hist        s              7  0 dim(13)
     d per_hist52      s              7  0 dim(52)
     d per_hist5       s              7  0 dim(52)
     d per_histw       s              7  0 dim(52)
     d pes_hist52      s              5  2 dim(52)
     d point           s              2  0
     d pointx          s              2  0
     d point1          s              2  0
     d point2          s              2  0
     d point3          s              2  0
     d point4          s              2  0
     d point5          s              2  0
     d point7          s              2  0
     d point8          s              2  0
     d new_slow        s              1  0                                      system stat N to S
      * --------------------------------------------------
     d prodhisb_key    DS                  likerec(rk_prodhis:*key)
     d prodh52b_key    DS                  likerec(rk_prodh52:*key)
      * --------------------------------------------------
      /free
       //------------------------------------------------------ Once Routine
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;

       exsr dcllccursor;
       //once routine
       exsr $_once;
       errors = *off;

       pf_chgdesc = *blanks;
       pf_chgdesc = 'Transfer supl ' +
                    %trim(zz_frlocn);

       add = *on;
       exsr $_hist_chk;

       //-------------------------------------------------- End of Main Loop

       //finished, set on LR
       *inlr = *on;
       //                return

       //**************************************************** End of program

       //////////////////////////////////////////////////////// Once routine

       begsr $_once;

       //automatically change 'N' to new products to 'S' slow
       //    new_slow   = 1 means that system status of 'N' changes to 'S'
       //    new_slow   = 0 means that no change made to system status
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
           where ta_comp = :wcomp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'K3S_8540  NEW_SLOW  '
           fetch first row only;
       if SQLState = SQLStateOk;
          new_slow = taflag1;
       else;
          new_slow = 0;
       endif;

       callp K3S_Retrieve_Timestamp(time_stamp);
       date = %date(%subst(%char(time_stamp):1:10):*ISO);

       sz_frlocn  = zz_frlocn;
       sz_tolocn  = zz_tolocn;

       //get product
       exec sql
         select *
           into :product_rec
           from k_product
           where pr_comp = :wcomp and
                 pr_locn = :zz_frlocn and
                 pr_supl = :zz_frsupl and
                 pr_suplsub = :zz_frsub and
                 pr_prod = :zz_frprod
           fetch first row only;

       zz_frforci = pr_forcint;
       zz_toforci = pr_forcint;

       endsr;

       /////////////////////////////////////////////////////// Edit product

       begsr $_edt_prod;

       //set indicator off
       errors = *off;

       if zz_frlocn = *blanks;
          exec sql
            select *
              into :product_rec
              from k_product
              where pr_comp = :wcomp and
                    pr_prod = :zz_frprod
              fetch first row only;
       else;
          exec sql
            select *
              into :product_rec
              from k_product
              where pr_comp = :wcomp and
                    pr_locn = :zz_frlocn and
                    pr_supl = :zz_frsupl and
                    pr_suplsub = :zz_frsub and
                    pr_prod = :zz_frprod
              fetch first row only;
       endif;

       // supplier record not found, send back error
       if SQLState = RowNotFound;
          errors = *on;
       else;
          zz_forcast = pr_forcast * zz_multply;
          zz_accsale = pr_accsale;
          zz_accouts = pr_accouts;
          zz_accdem  = pr_accdem;
          zz_fstslyr = pr_fstslyr;
          zz_fstslpr = pr_fstslpr;
          zz_frdelt  = pr_deltcnt;
          zz_frforci = pr_forcint;
       endif;

       if errors = *off;
       //get to supplier record
         if zz_tolocn = *blanks;
            exec sql
              select *
                into :product_rec
                from k_product
                where pr_comp = :wcomp and
                      pr_prod = :zz_toprod
              fetch first row only;
         else;
              exec sql
                select *
                  into :product_rec
                  from k_product
                  where pr_comp = :wcomp and
                        pr_locn = :zz_tolocn and
                        pr_supl = :zz_tosupl and
                        pr_suplsub = :zz_tosub and
                        pr_prod = :zz_toprod
                fetch first row only;
         endif;

       //   supplier record not found, send back error
         if SQLState = RowNotFound;
            errors    = *on;
         else;
            zz_toforci = pr_forcint;
            zz_todelt  = pr_deltcnt;
         endif;
       endif;

       if errors = *off;
          if zz_frforci <> zz_toforci;
             errors = *on;
          endif;
       endif;

       if errors = *off;
          if zz_frdelt > 0;
             errors = *on;
          endif;
       endif;

       if errors = *off;
          if zz_todelt > 0;
             errors = *on;
          endif;
       endif;

       endsr;


       //////////////////////////////////////////////// get history

       begsr $_hist_chk;

       domany = *off;
       if (sz_frlocn = *blanks) and
          (sz_tolocn = *blanks);
       //
          exsr InzInpSrch;
    �   //initialize StmtString
    �      exsr intSQLStmt;
    �   //prepare statement
    �      exsr prepDynSQLStmt;

          if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
             exsr opnlccursor;
       //fetch first k_locatns record from result set
             If SQLState = SQLStateOk;
                exec sql
                 fetch next
                  from lccursor
                  into :locatns_rec;

                  If SQLState = SQLStateOk;
                     zz_frlocn = lc_locn;
                     zz_tolocn = lc_locn;
                  endif;
             endif;
             exsr clslccursor;
          endif;
       endif;
       exsr $_edt_prod;
       if errors = *off;
         if zz_multply = 0;
            zz_multply = 1.0000;
         endif;
         if zz_frforci < 52;
           prodhisb_key.ph_comp = wcomp;
           prodhisb_key.ph_locn = zz_frlocn;
           prodhisb_key.ph_supl = zz_frsupl;
           prodhisb_key.ph_suplsub = zz_frsub;
           prodhisb_key.ph_prod = zz_frprod;
           prodhisb_key.ph_forcint = zz_frforci;
           prodhisb_key.ph_histype = zz_histype;
           setll %kds (prodhisb_key:7) k_prodhisb;
           reade(n) %kds (prodhisb_key:7) k_prodhisb;
           dow not %eof(k_prodhisb);

              per_hist(01) = ph_per01  * zz_multply;
              per_hist(02) = ph_per02  * zz_multply;
              per_hist(03) = ph_per03  * zz_multply;
              per_hist(04) = ph_per04  * zz_multply;
              per_hist(05) = ph_per05  * zz_multply;
              per_hist(06) = ph_per06  * zz_multply;
              per_hist(07) = ph_per07  * zz_multply;
              per_hist(08) = ph_per08  * zz_multply;
              per_hist(09) = ph_per09  * zz_multply;
              per_hist(10) = ph_per10  * zz_multply;
              per_hist(11) = ph_per11  * zz_multply;
              per_hist(12) = ph_per12  * zz_multply;
              per_hist(13) = ph_per13  * zz_multply;
              xx_year = ph_year;
              exsr $_hist_chg;
              xx_year = xx_year - 1;
              prodhisb_key.ph_comp = wcomp;
              prodhisb_key.ph_locn = zz_frlocn;
              prodhisb_key.ph_supl = zz_frsupl;
              prodhisb_key. ph_suplsub = zz_frsub;
              prodhisb_key.ph_prod = zz_frprod;
              prodhisb_key.ph_forcint = zz_frforci;
              prodhisb_key.ph_histype = zz_histype;
              prodhisb_key.ph_year = xx_year;
              chain(n) %kds (prodhisb_key) k_prodhisb;
              if (sz_frlocn = *blanks) and
                 (sz_tolocn = *blanks) and
                 not %found(k_prodhisb);
       //
                 exsr InzInpSrch;
    �   //initialize StmtString
    �             exsr intSQLStmt;
    �   //prepare statement
    �             exsr prepDynSQLStmt;

                 if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
                    exsr opnlccursor;
       //fetch first k_locatns record from result set
                    If SQLState = SQLStateOk;
                       exec sql
                        fetch next
                         from lccursor
                         into :locatns_rec;

                       If SQLState = SQLStateOk;
                          zz_frlocn = lc_locn;
                          zz_tolocn = lc_locn;
                          exsr clslccursor;
                          if pr_forcint < 52;
                             prodhisb_key.ph_comp = wcomp;
                             prodhisb_key.ph_locn = zz_frlocn;
                             prodhisb_key.ph_supl = zz_frsupl;
                             prodhisb_key.ph_suplsub = zz_frsub;
                             prodhisb_key.ph_prod = zz_frprod;
                             prodhisb_key.ph_forcint = zz_frforci;
                             prodhisb_key.ph_histype = zz_histype;
                             setll %kds (prodhisb_key:7) k_prodhisb;
                             reade(n) %kds (prodhisb_key:7) k_prodhisb;
                          endif;
                          domany = *off;
                       endif;
                    endif;
                 endif;
                 if SQLState = RowNotFound;
                    domany = *on;
                    leave;
                 endif;
              else;
                domany = *on;
              endif;
           enddo;
           add = *off;
         endif;


         if zz_frforci = 52;
             prodh52b_key.pw_comp = wcomp;
             prodh52b_key.pw_locn = zz_frlocn;
             prodh52b_key.pw_supl = zz_frsupl;
             prodh52b_key.pw_suplsub = zz_frsub;
             prodh52b_key.pw_prod = zz_frprod;
             prodh52b_key.pw_forcint = zz_frforci;
             prodh52b_key.pw_histype = zz_histype;
             setll %kds (prodh52b_key:7) k_prodh52b;
             reade(n) %kds (prodh52b_key:7) k_prodh52b;
             dow not %eof(k_prodh52b);

                 per_histw(01) = pw_per01 * zz_multply;
                 per_histw(02) = pw_per02 * zz_multply;
                 per_histw(03) = pw_per03 * zz_multply;
                 per_histw(04) = pw_per04 * zz_multply;
                 per_histw(05) = pw_per05 * zz_multply;
                 per_histw(06) = pw_per06 * zz_multply;
                 per_histw(07) = pw_per07 * zz_multply;
                 per_histw(08) = pw_per08 * zz_multply;
                 per_histw(09) = pw_per09 * zz_multply;
                 per_histw(10) = pw_per10 * zz_multply;
                 per_histw(11) = pw_per11 * zz_multply;
                 per_histw(12) = pw_per12 * zz_multply;
                 per_histw(13) = pw_per13 * zz_multply;
                 per_histw(14) = pw_per14 * zz_multply;
                 per_histw(15) = pw_per15 * zz_multply;
                 per_histw(16) = pw_per16 * zz_multply;
                 per_histw(17) = pw_per17 * zz_multply;
                 per_histw(18) = pw_per18 * zz_multply;
                 per_histw(19) = pw_per19 * zz_multply;
                 per_histw(20) = pw_per20 * zz_multply;
                 per_histw(21) = pw_per21 * zz_multply;
                 per_histw(22) = pw_per22 * zz_multply;
                 per_histw(23) = pw_per23 * zz_multply;
                 per_histw(24) = pw_per24 * zz_multply;
                 per_histw(25) = pw_per25 * zz_multply;
                 per_histw(26) = pw_per26 * zz_multply;
                 per_histw(27) = pw_per27 * zz_multply;
                 per_histw(28) = pw_per28 * zz_multply;
                 per_histw(29) = pw_per29 * zz_multply;
                 per_histw(30) = pw_per30 * zz_multply;
                 per_histw(31) = pw_per31 * zz_multply;
                 per_histw(32) = pw_per32 * zz_multply;
                 per_histw(33) = pw_per33 * zz_multply;
                 per_histw(34) = pw_per34 * zz_multply;
                 per_histw(35) = pw_per35 * zz_multply;
                 per_histw(36) = pw_per36 * zz_multply;
                 per_histw(37) = pw_per37 * zz_multply;
                 per_histw(38) = pw_per38 * zz_multply;
                 per_histw(39) = pw_per39 * zz_multply;
                 per_histw(40) = pw_per40 * zz_multply;
                 per_histw(41) = pw_per41 * zz_multply;

                 per_histw(42) = pw_per42 * zz_multply;
                 per_histw(43) = pw_per43 * zz_multply;
                 per_histw(44) = pw_per44 * zz_multply;
                 per_histw(45) = pw_per45 * zz_multply;
                 per_histw(46) = pw_per46 * zz_multply;
                 per_histw(47) = pw_per47 * zz_multply;
                 per_histw(48) = pw_per48 * zz_multply;
                 per_histw(49) = pw_per49 * zz_multply;
                 per_histw(50) = pw_per50 * zz_multply;
                 per_histw(51) = pw_per51 * zz_multply;
                 per_histw(52) = pw_per52 * zz_multply;
                 xx_year     = ph_year;
                 exsr $_hist_chg;
                 xx_year = xx_year - 1;
                 prodh52b_key.pw_comp = wcomp;
                 prodh52b_key.pw_locn = zz_frlocn;
                 prodh52b_key.pw_supl = zz_frsupl;
                 prodh52b_key.pw_suplsub = zz_frsub;
                 prodh52b_key.pw_prod = zz_frprod;
                 prodh52b_key.pw_forcint = zz_frforci;
                 prodh52b_key.pw_histype = zz_histype;
                 prodh52b_key.pw_year = xx_year;
                 chain(n) %kds (prodh52b_key) k_prodh52b;
                 if (sz_frlocn = *blanks) and
                    (sz_tolocn = *blanks) and
                    not %found(k_prodh52b);
       //
                    exsr InzInpSrch;
    �   //initialize StmtString
    �                exsr intSQLStmt;
    �   //prepare statement
    �                exsr prepDynSQLStmt;
       //
                    if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
                       exsr opnlccursor;
       //fetch first k_locatns record from result set
                       If SQLState = SQLStateOk;
                          exec sql
                           fetch next
                             from lccursor
                             into :locatns_rec;

                          If SQLSTT = SQLStateOk;
                             zz_frlocn = lc_locn;
                             zz_tolocn = lc_locn;
                             if pr_forcint = 52;
                                prodh52b_key.pw_comp = wcomp;
                                prodh52b_key.pw_locn = zz_frlocn;
                                prodh52b_key.pw_supl = zz_frsupl;
                                prodh52b_key.pw_suplsub = zz_frsub;
                                prodh52b_key.pw_prod = zz_frprod;
                                prodh52b_key.pw_forcint = zz_frforci;
                                prodh52b_key.pw_histype = zz_histype;
                                setll %kds (prodh52b_key:7) k_prodh52b;
                                reade(n) %kds (prodh52b_key:7) k_prodh52b;
                             endif;
                             domany = *off;
                          endif;
                       endif;
                       if SQLState = RowNotFound;
                         domany = *on;
                         leave;
                       endif;
                    endif;
                 else;
                      domany = *on;
                 endif;
             enddo;
             add = *off;
         endif;
       else;
          add = *off;
       endif;

       endsr;

       //////////////////////////////////////////////// chg history

       begsr $_hist_chg;

       if (sz_frlocn = *blanks) and
         (sz_tolocn = *blanks) or
         (sz_frlocn <> *blanks) and
         (sz_tolocn <> *blanks);
        //edit product entry
            exsr $_edt_prod;
            if (errors = *off) and
               (zz_frforci = zz_toforci) and
               (zz_frdelt = 0) and
               (zz_todelt = 0);
               if add = *on;

                  if pr_forcint < 52;
                     prodhisb_key.ph_comp = wcomp;
                     prodhisb_key.ph_locn = zz_tolocn;
                     prodhisb_key.ph_supl = zz_tosupl;
                     prodhisb_key.ph_suplsub = zz_tosub;
                     prodhisb_key.ph_prod = zz_toprod;
                     prodhisb_key.ph_forcint = zz_toforci;
                     prodhisb_key.ph_histype = zz_histype;
                     prodhisb_key.ph_year = xx_year;
                     chain %kds (prodhisb_key) k_prodhisb;
                     if add = *on;
                       ph_per01 = ph_per01 + per_hist(01);
                       ph_per02 = ph_per02 + per_hist(02);
                       ph_per03 = ph_per03 + per_hist(03);
                       ph_per04 = ph_per04 + per_hist(04);
                       ph_per05 = ph_per05 + per_hist(05);
                       ph_per06 = ph_per06 + per_hist(06);
                       ph_per07 = ph_per07 + per_hist(07);
                       ph_per08 = ph_per08 + per_hist(08);
                       ph_per09 = ph_per09 + per_hist(09);
                       ph_per10 = ph_per10 + per_hist(10);
                       ph_per11 = ph_per11 + per_hist(11);
                       ph_per12 = ph_per12 + per_hist(12);
                       ph_per13 = ph_per13 + per_hist(13);
                     endif;
                     if %found(k_prodhisb);
                        ph_lastupd = date;
                        update rk_prodhis;
                     else;
                        ph_lastupd = date;
                        ph_birth = date;
                        ph_comp = wcomp;
                        ph_locn = zz_tolocn;
                        ph_supl = zz_tosupl;
                        ph_suplsub = zz_tosub;
       //   081697       eval      ph_suplusr = zz_tosupl
                        ph_suplusr = pr_suplusr;
                        ph_suplusb = pr_suplusb;
                        ph_prod = zz_toprod;
                        ph_year = xx_year;
                        ph_forcint = zz_frforci;
                        write rk_prodhis;
                     endif;

                     if domany = *off;
                         callp K3S_Retrieve_Timestamp(time_stamp);
                         time = %time(time_stamp);
                         date = %date(%subst(%char(time_stamp):1:10):*ISO);
                         exec sql
                           select *
                           into :product_rec
                           from k_product
                           where pr_comp = :wcomp and
                                 pr_locn = :zz_frlocn and
                                 pr_supl = :zz_frsupl and
                                 pr_suplsub = :zz_frsub and
                                 pr_prod = :zz_frprod
                           fetch first row only;
                         pf_comp = wcomp;
                         pf_locn = pr_locn;
                         pf_supl = pr_supl;
                         pf_suplsub = pr_suplsub;
                         pf_prod = pr_prod;
                         pf_chgtype = 'T';
                         pf_user = psds_user;
                         pf_workstn = wrk_statn;
                         pf_program = psds_progm;
                         pf_birth = date;
                         pf_birthtm = time;
                         if add = *on;
                            zz_forcast += pr_forcast;
                            zz_accsale += pr_accsale;
                            zz_accouts += pr_accouts;
                            zz_accdem  += pr_accdem;
                         endif;
                         pf_avgdiff = zz_forcast - pr_forcast;
                         pf_avgbef  = pr_forcast;
                         pf_avgaft  = zz_forcast;
                         pf_devpbef = pr_fordevp;
                         pf_devpaft = pr_fordevp;
                         pf_seasbef = pr_seasonl;
                         pf_seasaft = pr_seasonl;
                         pf_statbef = pr_usrstat;
                         pf_stataft = pr_usrstat;
                         pf_sysstat = pr_sysstat;
                         if pr_sysstat = 'N' AND new_slow = 1;
                            pr_sysstat = 'S';
                         endif;
                         pr_forcast = zz_forcast;
                         pr_accsale = zz_accsale;
                         pr_accouts = zz_accouts;
                         if        pr_fstslyr > zz_fstslyr and
                                   zz_fstslyr > 0;
                                   pr_fstslyr = zz_fstslyr;
                                   pr_fstslpr = zz_fstslpr;
                         endif;
                         if        (pr_fstslyr = zz_fstslyr) and
                                   (pr_fstslpr > zz_fstslpr);
                                   pr_fstslpr = zz_fstslpr;
                         endif;
                         if        (pr_fstslyr = 0) and
                                   (pr_fstslpr = 0);
                                   pr_fstslyr = zz_fstslyr;
                                   pr_fstslpr = zz_fstslpr;
                         endif;
                         pr_forchg = 'T';
                         pr_accdem  = zz_accdem;
                         pr_formanl = date;
                         pr_lastupd = date;
                         pr_ansale$ = pr_forcast * pr_sales *
                                      pr_forcint;
                         pr_ansaleu = pr_forcast * pr_forcint;
                         exec sql
                           update k_product
                           set pr_sysstat = :pr_sysstat,
                               pr_forcast = :pr_forcast,
                               pr_accsale = :pr_accsale,
                               pr_accouts = :pr_accouts,
                               pr_fstslyr = :pr_fstslyr,
                               pr_fstslpr = :pr_fstslyr,
                               pr_forchg  = :pr_forchg,
                               pr_accdem  = :pr_accdem,
                               pr_formanl = :pr_formanl,
                               pr_lastupd = :pr_lastupd,
                               pr_ansale$ = :pr_ansale$,
                               pr_ansaleu = :pr_ansaleu
                           where pr_comp = :wcomp and
                                 pr_locn = :zz_frlocn and
                                 pr_supl = :zz_frsupl and
                                 pr_suplsub = :zz_frsub and
                                 pr_prod = :zz_frprod;
                         exsr $_wrt_prod;
                     endif;
                  endif;

                  if pr_forcint = 52;
                     prodhisb_key.ph_comp = wcomp;
                     prodhisb_key.ph_locn = zz_tolocn;
                     prodhisb_key.ph_supl = zz_tosupl;
                     prodhisb_key.ph_suplsub = zz_tosub;
                     prodhisb_key.ph_prod = zz_toprod;
                     prodhisb_key.ph_forcint = zz_toforci;
                     prodhisb_key.ph_histype = zz_histype;
                     prodhisb_key.ph_year = xx_year;
                     chain %kds (prodh52b_key) k_prodh52b;
                     if add = *on;
                         pw_per01 = pw_per01 + per_histw(01);
                         pw_per02 = pw_per02 + per_histw(02);
                         pw_per03 = pw_per03 + per_histw(03);
                         pw_per04 = pw_per04 + per_histw(04);
                         pw_per05 = pw_per05 + per_histw(05);
                         pw_per06 = pw_per06 + per_histw(06);
                         pw_per07 = pw_per07 + per_histw(07);
                         pw_per08 = pw_per08 + per_histw(08);
                         pw_per09 = pw_per09 + per_histw(09);
                         pw_per10 = pw_per10 + per_histw(10);
                         pw_per11 = pw_per11 + per_histw(11);
                         pw_per12 = pw_per12 + per_histw(12);
                         pw_per13 = pw_per13 + per_histw(13);
                         pw_per14 = pw_per14 + per_histw(14);
                         pw_per15 = pw_per15 + per_histw(15);
                         pw_per16 = pw_per16 + per_histw(16);
                         pw_per17 = pw_per17 + per_histw(17);
                         pw_per18 = pw_per18 + per_histw(18);
                         pw_per19 = pw_per19 + per_histw(19);
                         pw_per20 = pw_per20 + per_histw(20);
                         pw_per21 = pw_per21 + per_histw(21);
                         pw_per22 = pw_per22 + per_histw(22);
                         pw_per23 = pw_per23 + per_histw(23);
                         pw_per24 = pw_per24 + per_histw(24);
                         pw_per25 = pw_per25 + per_histw(25);
                         pw_per26 = pw_per26 + per_histw(26);
                         pw_per27 = pw_per27 + per_histw(27);
                         pw_per28 = pw_per28 + per_histw(28);
                         pw_per29 = pw_per29 + per_histw(29);
                         pw_per30 = pw_per30 + per_histw(30);
                         pw_per31 = pw_per31 + per_histw(31);
                         pw_per32 = pw_per32 + per_histw(32);
                         pw_per33 = pw_per33 + per_histw(33);
                         pw_per34 = pw_per34 + per_histw(34);
                         pw_per35 = pw_per35 + per_histw(35);
                         pw_per36 = pw_per36 + per_histw(36);
                         pw_per37 = pw_per37 + per_histw(37);
                         pw_per38 = pw_per38 + per_histw(38);
                         pw_per39 = pw_per39 + per_histw(39);
                         pw_per40 = pw_per40 + per_histw(40);
                         pw_per41 = pw_per41 + per_histw(41);
                         pw_per42 = pw_per42 + per_histw(42);
                         pw_per43 = pw_per43 + per_histw(43);
                         pw_per44 = pw_per44 + per_histw(44);
                         pw_per45 = pw_per45 + per_histw(45);
                         pw_per46 = pw_per46 + per_histw(46);
                         pw_per47 = pw_per47 + per_histw(47);
                         pw_per48 = pw_per48 + per_histw(48);
                         pw_per49 = pw_per49 + per_histw(49);
                         pw_per50 = pw_per50 + per_histw(50);
                         pw_per51 = pw_per51 + per_histw(51);
                         pw_per52 = pw_per52 + per_histw(52);
                     endif;
                     if %found (k_prodh52b);
                         pw_lastupd = date;
                         update rk_prodh52;
                     else;
                         pw_lastupd = date;
                         pw_birth   = date;
                         pw_comp    = wcomp;
                         pw_locn    = zz_tolocn;
                         pw_supl    = zz_tosupl;
                         pw_suplsub = zz_tosub;
        //   081697      eval      pw_suplusr = zz_tosupl
                         pw_suplusr = pr_suplusr;
                         pw_suplusb = pr_suplusb;
                         pw_prod    = zz_toprod;
                         pw_year    = xx_year;
                         pw_forcint = zz_frforci;
                         write rk_prodh52;
                     endif;
                     if domany = *off;
                        callp K3S_Retrieve_Timestamp(time_stamp);
                        time = %time(time_stamp);
                        date = %date(%subst(%char(time_stamp):1:10):*ISO);
                        exec sql
                          select *
                            into :product_rec
                            from k_product
                            where pr_comp = :wcomp and
                                  pr_locn = :zz_frlocn and
                                  pr_supl = :zz_frsupl and
                                  pr_suplsub = :zz_frsub and
                                  pr_prod = :zz_frprod
                                  fetch first row only;
                        pf_comp = wcomp;
                        pf_locn = pr_locn;
                        pf_supl = pr_supl;
                        pf_suplsub = pr_suplsub;
                        pf_prod = pr_prod;
                        pf_chgtype = 'T';
                        pf_user = psds_user;
                        pf_workstn = wrk_statn;
                        pf_program = psds_progm;
                        pf_birth = date;
                        pf_birthtm = time;
                        if add = *on;
                          zz_forcast = zz_forcast + pr_forcast;
                          zz_accsale = zz_accsale + pr_accsale;
                          zz_accouts = zz_accouts + pr_accouts;
                          zz_accdem  = zz_accdem  + pr_accdem;
                        endif;
                        pf_avgdiff = zz_forcast - pr_forcast;
                        pf_avgbef  = pr_forcast;
                        pf_avgaft  = zz_forcast;
                        pf_devpbef = pr_fordevp;
                        pf_devpaft = pr_fordevp;
                        pf_seasbef = pr_seasonl;
                        pf_seasaft = pr_seasonl;
                        pf_statbef = pr_usrstat;
                        pf_stataft = pr_usrstat;
                        pf_sysstat = pr_sysstat;
                        if pr_sysstat = 'N' and new_slow = 1;
                            pr_sysstat = 'S';
                        endif;
                        pr_forcast = zz_forcast;
                        pr_accsale = zz_accsale;
                        pr_accouts = zz_accouts;
                        pr_accdem  = zz_accdem;
                        if        pr_fstslyr > zz_fstslyr and
                                   zz_fstslyr > 0;
                            pr_fstslyr = zz_fstslyr;
                            pr_fstslpr = zz_fstslpr;
                        endif;
                        if (pr_fstslyr = zz_fstslyr) and
                           (pr_fstslpr > zz_fstslpr);
                              pr_fstslpr = zz_fstslpr;
                        endif;
                        if (pr_fstslyr = 0) and
                           (pr_fstslpr = 0);
                               pr_fstslyr = zz_fstslyr;
                               pr_fstslpr = zz_fstslpr;
                        endif;
                        pr_formanl = date;
                        pr_forchg = 'T';
                        pr_lastupd = date;
                        pr_ansale$ = pr_forcast * pr_sales *
                                     pr_forcint;
                        pr_ansaleu = pr_forcast * pr_forcint;
                        exec sql
                        update k_product
                          set pr_sysstat = :pr_sysstat,
                              pr_forcast = :pr_forcast,
                              pr_accsale = :pr_accsale,
                              pr_accouts = :pr_accouts,
                              pr_fstslyr = :pr_fstslyr,
                              pr_fstslpr = :pr_fstslyr,
                              pr_forchg  = :pr_forchg,
                              pr_accdem  = :pr_accdem,
                              pr_formanl = :pr_formanl,
                              pr_lastupd = :pr_lastupd,
                              pr_ansale$ = :pr_ansale$,
                              pr_ansaleu = :pr_ansaleu
                          where pr_comp = :wcomp and
                                pr_locn = :zz_frlocn and
                                pr_supl = :zz_frsupl and
                                pr_suplsub = :zz_frsub and
                                pr_prod = :zz_frprod;
                        exsr $_wrt_prod;
                     endif;
                  endif;
               endif;
            endif;
       endif;

       endsr;

       begsr dcllccursor;
       exec sql
        declare lccursor Cursor
         for DynSQLStmt;
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Locatns +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'lc_comp = ? +
                     Order by lc_comp, +
                              lc_locn';
       endsr;

       begsr opnlccursor;
       exec sql
        open lccursor
          using :wcomp;
       endsr;

       begsr clslccursor;
       exec sql
        close lccursor;
       endsr;

       ////////////////////////////////////////////// Write k_prodfor

       begsr $_wrt_prod;

       callp K3S_8547(wcomp:
                      pf_locn:
                      pf_supl:
                      pf_suplsub:
                      pf_prod:
                      pf_birth:
                      pf_birthtm:
                      pf_chgtype:
                      pf_chgdesc:
                      pf_avgbef:
                      pf_avgaft:
                      pf_avgdiff:
                      pf_seasbef:
                      pf_seasaft:
                      pf_devpbef:
                      pf_devpaft:
                      pf_statbef:
                      pf_stataft:
                      pf_sysstat:
                      pf_user:
                      pf_workstn:
                      pf_program);
       endsr;

      /end-free

      * ////////////////////////////////////////////////////////////////////
