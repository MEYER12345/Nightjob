      *****************************************************************
     h copyright('(C) Copyright 1996 - 2003 King III Solutions, Inc.  +
     h Rel 4.31 2003-01-03       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

      *DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)
     h AUT(*ALL)
     h bnddir('QC2LE')

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2003 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_1021
      **   Type: ILE RPG Program
      **   Desc: Supplier suggested order list
      **
      *****************************************************************
      **
      **  This program is used to print the supplier suggested order
      **  report.
      **
      *****************************************************************

      *_prodsoq if   e           k disk
      * product suggested order quantity

      *_companyaif   e           k disk
      * companys by company id

      *_suplsoqaif   e           k disk
      * supplier suggested order

      *_locatnsaif   e           k disk
      * locations by locn

      *_buyrgrpaif   e           k disk                                         buy groups
      * buy group by buy group

      *_productaif   e           k disk
      * products

     fk3s_p1021 o    e             printer oflind(*in55)
      * printer file

      * --------------------------------------------------- parameter passed prototype
     d K3S_1021        PR
     d  comp                          1
     d  buyr                          5
     d  locn                          5
     d  supl                         10
     d  suplsub                      10
     d  soqseq#                       5p 0
     d  ponbr                        10
      * ----------------------------------------------------- procedure interface
     d K3S_1021        PI
     d  comp                          1
     d  buyr                          5
     d  locn                          5
     d  supl                         10
     d  suplsub                      10
     d  soqseq#                       5p 0
     d  ponbr                        10
      * ----------------------------------------------------- get time stamp
     d lda_usrdat      s              4                                         user date format
     d lda_usrtim      s              4                                         user time format
     d lda_usradj      s              3p 0                                      user time adj. hours

      * --------------------------------------------------------- Workfields
      * --------------------------------------------------------- Workfields
     d MyDS            ds
     d  prod                               like(pq_prod)
     d  desc1                              like(pq_desc1)
     d  costdiv                            like(pq_costdiv)
     d  costord                            like(pq_costord)
     d  soqact                             like(pq_soqact)
      * --------------------------------------------------------- Workfields
     d MyD2            ds
     d  name                               like(so_name)
      * --------------------------------------------------------- Workfields
     d MyD3            ds
     d  ndcupc                             like(pr_ndc_upc)
      * --------------------------------------------------------- Workfields
     d MyD4            ds
     d  compn                              like(cm_comp)
     d  cmpcod                             like(cm_compcod)
     d  cmpname                            like(cm_cmpname)
      * --------------------------------------------------------- Workfields
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
      * --------------------------------------------------------- Workfields
     d buyrgrp_rec   e ds                  ExtName(k_buyrgrp)
      * --------------------------------------------------------- Workfields
     d locatns_rec   e ds                  ExtName(k_locatns)
      * --------------------------------------------------------- Workfields
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
      * --------------------------------------------------------- Workfields
     d product_rec   e ds                  ExtName(k_product)
      * --------------------------------------------------------- Workfields
     d company_rec   e ds                  ExtName(k_company)
      * --------------------------------------------------------- Workfields
     d                 ds
     d wcomp                          1
      * --------------------------------------------------------- Workfields
     d time_stamp      s               z   inz                                  time stamp
     d time            s               t                                        time
     d date            s               d                                        date
     d x               s              3p 0                                      # valid locations
     d y               s              3p 0                                      step through locns
     d first_page      s              1    inz(*off)                            first page printed
     d filename        s             10    inz('k_companya')
     d libname         s             10    inz('k3s_5dta')
     d*wcomp           s              1                                         date
     d wbuyr           s              5                                         date
     d wlocn           s              5                                         date
     d wsupl           s             10                                         date
     d wsuplsub        s             10                                         date
     d wsoqseq#        s              5p 0                                      soq sequence #
     d wponbr          s             10
      * --------------------------------------------------- Named indicators
      * --------------------------------------------------- Named indicators
     d over_flow       c                   55                                   print overflow

      * --------------------------------------------------- parameter passed
      *soqseq#         s              5p 0                                      soq sequence #

      * -------------------------------------- Program Status Data Structure
     d*copy k3s_c040

      * -------------------------------------- Likerec Statements
     d*companya_key    DS                  likerec(rk_company:*key)
     d*suplsoqa_key    DS                  likerec(rk_suplsoq:*key)
     d*prodsoqh_key    DS                  likerec(rk_prodsoq:*key)
     d*buyrgrpa_key    DS                  likerec(rk_buyrgrp:*key)
     d*locatnsa_key    DS                  likerec(rk_locatns:*key)
     d*producta_key    DS                  likerec(rk_product:*key)
      *****************************************************************
      *(K3S_M090;'
      **   K3S-Replenish - Inventory REPLENISHment System
      **   Copyright (c) 1996-1997 by King III Solutions, Inc.
      **   All rights reserved.
      **
      *****************************************************************
      **
      **   Name: K3S_C040
      **   Type: ILE /COPY member
      **   Desc: *PROC Program Status Data Structure fields
      **
      *****************************************************************
     d                sds
     d psds_progm        *proc                                                  program name
     d psds_error             90    170                                         error
     d psds_user             254    263                                         user ID

      * --------------------------------------------------- parameter passed
     d K3S_M090        PR                                                       soq sequence #
     d  time_stamp                     z
      * -------------------------------------------------- Parameters passed
     d K3S_M120        PR                                                       soq sequence #
     d  lda_usrdat                    4
     d  lda_usrtim                    4
     d  lda_usradj                    3p 0
     d  lda_usrdate                   4
     d  lda_usrtime                   4
      * -------------------------------------------------- Parameters passed
     d CmdToRun        pr            10i 0 extproc('system')
     d Cmd                             *   value options(*string)
      * -------------------------------------------------- Parameters passed
     d CmdRunResult    s             10i 0
     d Command         s            100a
      * -------------------------------------------------- Parameters passed
     c/exec sql
     c+ set option closqlcsr = *endmod
     c/end-exec
      * parameters passed to program
      *    *entry        plist
      *                  parm                    comp
      *                  parm                    buyr
      *                  parm                    locn
      *                  parm                    supl
      *                  parm                    suplsub
      *                  parm                    soqseq#
      *                  parm                    ponumber
      * key list for product suggested order
     c     pq_key        klist
     c                   kfld                    pq_comp
     c                   kfld                    pq_buyr
     c                   kfld                    pq_locn
     c                   kfld                    pq_supl
     c                   kfld                    pq_suplsub
     c                   kfld                    pq_soqseq#

      * ---------------------------------------------------------- Key Lists
      * --------------------------------------------------- prime key fields
      ** prime key fields company
      /free
       pq_comp = comp;
       pq_buyr = buyr;
       pq_locn = locn;
       pq_supl = supl;
       pq_suplsub = suplsub;
       pq_soqseq# = soqseq#;

       //plsoqa_key.so_comp = comp;
       //prodsoqh_key.pq_comp = comp;
       //oducta_key.pr_comp = comp;
       //catnsa_key.lc_comp = comp;
       //yrgrpa_key.by_comp = comp;

       //prime key fields buy group
       //plsoqa_key.so_buyr = buyr;
       //prodsoqh_key.pq_buyr = buyr;
       //yrgrpa_key.by_buyr = buyr;

       //prime key fields location
       //plsoqa_key.so_locn = locn;
       //prodsoqh_key.pq_locn = locn;
       //oducta_key.pr_locn = locn;
       //catnsa_key.lc_locn = locn;

       //prime key fields supplier
       //plsoqa_key.so_supl = supl;
       //prodsoqh_key.pq_supl = supl;
       //oducta_key.pr_supl = supl;

       //prime key fields sub supplier
       //plsoqa_key.so_suplsub = suplsub;
       //plsoqa_key.so_suplsub = suplsub;
       //prodsoqh_key.pq_suplsub = suplsub;
       //oducta_key.pr_suplsub = suplsub;

       //prime key fields suggested order sequence #
       //plsoqa_key.so_soqseq# = soqseq#;
       //prodsoqh_key.pq_soqseq# = soqseq#;

       //prime key fields buy group
       //yrgrpa_key.by_buyr = buyr;
       wcomp = comp;
       //ply wcomp;
       wbuyr = buyr;
       wlocn = locn;
       wsupl = supl;
       wsuplsub = suplsub;
       wsoqseq# = soqseq#;
       wponbr = ponbr;
       //--------------------------------------------------------- main line
       //get header information
       //ain %kds(suplsoqa_key) k_suplsoqa;
       // %found(k_suplsoqa);
       // supl_desc = %trimr(supl) + ' ' + suplsub
       //                          + ' ' + so_name;
       //dif;
      /end-free
     c*
     c/exec sql
     c+  select so_name
     c+    into :MyD2
     c+    from k_suplsoq
     c+  where so_comp = :wcomp and so_buyr = :wbuyr and so_locn = :wlocn and
     c+  so_supl = :wsupl and so_suplsub = :wsuplsub and so_soqseq# = :wsoqseq#
     c/end-exec
      /free
         If SQLSTT = '00000';
          supl_desc = %trimr(supl) + ' ' + suplsub
                                   + ' ' + name;
         Endif;
       //GET BUY GROUP RECORD

       //ain %kds(buyrgrpa_key) k_buyrgrpa;
       // %found(k_buyrgrpa);
       //  buy_group = %trimr(buyr) + ' ' + by_name;
       //dif;

      /end-free
     c*
     c/exec sql
     c+  select *
     c+    into :buyrgrp_rec
     c+    from k_buyrgrp
     c+  where by_comp = :wcomp and by_buyr = :wbuyr
     c/end-exec
      /free
         If SQLSTT = '00000';
           buy_group = %trimr(buyr) + ' ' + by_name;
         Endif;
      /end-free
     c*
     c/exec sql
     c+  select *
     c+    into :locatns_rec
     c+    from k_locatns
     c+  where lc_comp = :wcomp and lc_locn = :wlocn
     c/end-exec
      /free
         If SQLSTT = '00000';
          locn_desc = %trimr(locn) + ' ' + lc_desc;
         Endif;


       //GET LOCATION DESCRIPTION
       //ain %kds(locatnsa_key) k_locatnsa;
       // %found(k_locatnsa);
       // locn_desc = %trimr(locn) + ' ' + lc_desc;
       //dif;

       //set off indicator 11
       //mmand = 'chkobj' + %trimr(libname) + '/' +
       //        %trimr(filename) + '*file';

       //ply filename;

       //dRunRetselectCmdtoRun(Command);
       //ply CmdRunResult;
       // CmdRunResult = 0;
       // dsply (filename) 'found';
       //se;
       // dsply (filename) 'not found';
       //dif;

      /end-free
     c*
     c/exec sql
     c+ declare mainCursor Cursor
     c+   for
     c+  select pq_prod, pq_desc1, pq_costdiv, pq_costord, pq_soqact
     c+    from k_prodsoq
     c+  where pq_comp = :wcomp and pq_buyr = :wbuyr and pq_locn = :wlocn and
     c+     pq_supl = :wsupl and pq_suplsub = :wsuplsub and
     c+     pq_soqseq# = :wsoqseq#
     c+   order by pq_comp,pq_buyr,pq_locn,pq_supl,pq_suplsub,pq_soqseq#
     c+   for fetch only
     c/end-exec
     c/exec sql
     c+ open mainCursor
     c/end-exec
      /free
       //sply SQLSTT;
      /end-free
     c/exec sql
     c+ fetch next
     c+  from mainCursor
     c+ into :myDS
     c/end-exec
      /free

       //sply SQLSTT;
       //set off indicator 11
       //  *in11 = *off;

       //loopthrough all products in this order
       //sply SQLSTT;
         Dow SQLSTT = '00000';
       //free
        //nly process when record read, and soq exists
        // if *in11 = *off and pq_soqact > 0;

          if soqact > 0;

              tot_record = tot_record + 1;

        //get ndc_upc id
              prprod = prod;
       //oducta_key.pr_prod = prod;
       //ain %kds(suplsoqa_key) k_suplsoqa;
      /end-free
     c*
     c/exec sql
     c+  select so_name
     c+    into :MyD2
     c+    from k_suplsoq
     c+  where so_comp = :wcomp and so_buyr = :wbuyr and so_locn = :wlocn and
     c+  so_supl = :wsupl and so_suplsub = :wsuplsub and so_soqseq# = :wsoqseq#
     c/end-exec

       //ain %kds(producta_key) k_producta;
     c/exec sql
     c+  select pr_ndc_upc
     c+    into :myd3
     c+    from k_product
     c+  where pr_comp = :wcomp and pr_locn = :wlocn and pr_supl = :wsupl and
     c+  pr_suplsub = :wsuplsub and pr_prod = :prod
     c/end-exec
      /free
        //ply prod;
        //ply ndcupc;
            //print product
           exsr $_print;

           endif;
      /end-free
     c/exec sql
     c+ fetch next
     c+  from mainCursor
     c+ into :myDS
     c/end-exec
      /free
           enddo;

        //----------------------------------------------------- end products

           *inlr = '1';

         //print total line
           if *inlr = *on;
              write k3s_r03;
           Endif;
      /end-free
     c/exec sql
     c+ close mainCursor
     c/end-exec
      /free
         ///////////////////////////////////////////////////////////// Print

           begsr $_print;

          //print header on fist page or overflow
            if first_page = *off  or *in(over_flow) = *on;
               first_page = *on;
               *in(over_flow) = *off;
               write     k3s_r01;
            endif;                                                //eader page

          //calculate dollar ertension
            //dsply soqact;
            //dsply costord;
            //dsply costdiv;
            extension = soqact *
                        costord/costdiv;
            prsoqact = soqact;
            prcostdiv = costdiv;
            prcostord = costord;
            prndc_upc = ndcupc;
            pq_desc1x = desc1;

          //print detail line
            write k3s_r02;

          //accumulate total dollars
            total_$ = total_$ + extension;

            endsr;

           //////////////////////////////////////////////////////// One time

           begsr *inzsr;
       wcomp = comp;
      /end-free
     c*
     c/exec sql
     c+  select cm_comp, cm_compcod, cm_cmpname
     c+    into :MyD4
     c+    from k_company
     c+  where cm_comp = :wcomp
     c/end-exec
      /free
            //ply wcomp;
           //get company code
           //_comp = comp;
           //ain (cm_comp) rk_company;

           //prime program id
             zz_program = psds_progm;

           //prime company code
             //ply cmpcod;
             zz_compcod = cmpcod;

           //prime user id
             zz_user = psds_user;

           //report title
             zz_title  = 'Suggested Order Report  ';
             zz_title2 = cmpname;

           //------------------------------------------------- get timestamp
           //call subprocedure to retrieve time stamp
             callp K3S_M090 (time_stamp);

           //--------------------------------------------- get time formated
             lda_usrdat = cm_reptdat;
             lda_usrtim = cm_repttim;
             lda_usradj = 0;
           //get time formatted
             exsr $_get_time;

             endsr;
      /end-free

      * -----------------------------------------get time subroutine
     c*copy k3s_c180
      *****************************************************************
      **
      **   K3S-Replenish - Inventory REPLENISHment System
      **   Copyright (c) 1996-1997 by King III Solutions, Inc.
      **   All rights reserved.
      **
      *****************************************************************
      **
      **   Name: K3S_C180
      **   Type: ILE /COPY member
      **   Desc: Get system date and time formated       'C' specs only
      **
      *****************************************************************

      * //////////////////////////////////////// Get date and time formated

      /free
        begsr $_get_time;

       //-------------------------------------------- Retrieve date and time
       //callmodule to retrieve user formated date and time
        callp K3S_M120 (lda_usrdat:lda_usrtim:lda_usradj:zz_usrdate:zz_usrtime);

        endsr;
      /end-free


