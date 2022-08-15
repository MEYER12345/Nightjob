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
      **   Name: K3S_4020
      **   Type: ILE RPG Program
      **   Desc: Buy Group period end filtering of products
      **
      *****************************************************************
      **
      **  This program is used to build a file of period end filtered
      **  products.
      **
      *****************************************************************
      **
      **  This program reads products from file k_product that were
      **  assembled using OPNQRYF. This OPNQRYF set should only contain
      **  data for one company at a time, and includes any products
      **  that have an end of period exception. Multiple locations can be
      **  processed during the same batch. For performance, make sure
      **  sequence is in company, buy group, location, check type.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 08/17/2014.
      *   Remarks. Removed OPYQRYF CL statements pertaining to K3S_4020
      *            from K3S_NITECL and left the call to RPG program
      *            K3S_4020. In K3S_4020 program added code to declare
      *            cursor prcursor to do the selecting and sorting
      *            formerly done in K3S_NITECL. Changed to use cursor
      *            prcursor to loop through k_product result set.
      *            Added SQL SELECT statements to access K_SUPLIER,
      *            K_LOCATNS, and K_BUYRFIL.  Also, added an SQL insert
      *            statement to write K_PRODFLT records.
      *****************************************************************
      * -------------------------------------------------------- work fields
     d zz_histype      s              1  0
     d zz_hist01       s              7  0
     d zz_hist02       s              7  0
     d zz_hist03       s              7  0
     d zz_hist04       s              7  0
     d zz_hist05       s              7  0
     d zz_hist06       s              7  0
     d zz_hist07       s              7  0
     d zz_hist08       s              7  0
     d zz_hist09       s              7  0
     d zz_hist10       s              7  0
     d zz_hist11       s              7  0
     d zz_hist12       s              7  0
     d zz_hist13       s              7  0
     d filter          s              1                                         filter products
     d diff_units      s             15  2                                      difference units
     d diff_dolrs      s             15  2                                      difference dollars
     d diff_days       s              7  0                                      difference days
     d per01           s              7  0                                      period 01
     d per02           s              7  0                                      period 02
     d per03           s              7  0                                      period 03
     d per_hist        s              7  0 dim(13)                              demand history
     d point           s              3  0                                      point to period
     d count           s              1  0                                      count 3 periods
     d spname          s             40                                         supplier name
     d lcsysdate       s               d                                        location system date
     d testname        s             40
     d testdate        s               d
     d bfcomp          s              1
     d bfbuyr          s              5
     d bflocn          s              5
     d bffltunit       s              5  2
     d bffltdolr       s              5  2
     d bflast30        s              1  0
     d bfchktyp3       s              3
     d locn            s              5                                         location saved
     d ptexceptn       s              1  0                                      location saved
     d BuyrFileNOT     s               n                                        Filter record - no
     d recs_read       s              7  0
     d recs_written    s              7  0
     d
      * ----------------------------------------- Supplier for Control Break
     d                 ds
     d supl                          25
     d  xx_locn                       5    overlay(supl:1)                      location
     d  xx_supl                      10    overlay(supl:6)                      supplier
     d  xx_suplsub                   10    overlay(supl:16)                     supplier sub
     d suplsaved       s                   like(supl)

      * -------------------------------- Buy group/location/check type break
     d                 ds
     d buyrlocchk                    11
     d  prbuyr                        5    overlay(buyrlocchk:1)                buy group
     d  prlocn                        5    overlay(buyrlocchk:6)                location
     d  prendper                      1    overlay(buyrlocchk:11)               period end check typ
     d buyloccksv      s                   like(buyrlocchk)
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d product_rec   e ds                  ExtName(k_product)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------  procedure interface
     d K3S_4020        PI
     d  comp                          1
      /free

       //-------------------------------------------------- Read Products
        exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclprcursor;
       exsr clsprcursor;
       exsr opnprcursor;

       //-------------------------------------------------------- Main Loop
       //main loop
       //--------------------------------------------------------- Main Loop
       dow SQLState = SQLStateOk;

       //read product row from cursor
          exec sql
           fetch next
            from prcursor
            into :product_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

          recs_read += 1;

          xx_locn = pr_locn;
          xx_supl = pr_supl;
          xx_suplsub = pr_suplsub;
       //---------------------------------------------------- Supplier break
       //change in supplier ID
          if      supl <> suplsaved;

       //save new supplier ID
                  suplsaved =     supl;

       //get supplier record

                  Exec sql
                    Select sp_name
                      Into :spname
                      From k_suplier
                      Where sp_comp = :pr_comp and
                            sp_locn = :pr_locn and
                            sp_supl = :pr_supl and
                            sp_suplsub = :pr_suplsub
                      Fetch first row only;
          endif;

          testname = spname;
       //---------------------------------------------------- Location break
       //change in location
          if      pr_locn <> locn;
                  locn    = pr_locn;

       //get location

                  Exec sql
                    Select lc_sysdate
                      Into :lcsysdate
                      From k_locatns
                      Where lc_comp = :pr_comp and
                            lc_locn = :pr_locn
                      Fetch first row only;
          endif;

       //--------------------------  Buy group / location / check type break

       //change in buy group / location / check type

          prbuyr    = pr_buyr;
          prlocn    = pr_locn;
          prendper  = pr_endper;

          if       buyrlocchk <> buyloccksv;
                   buyloccksv =  buyrlocchk;

                   bfcomp    =  pr_comp;
                   bfbuyr    =  pr_buyr;
                   bflocn    =  pr_locn;
                   bfchktyp3 =  'PE' + pr_endper;

       //get buy group/location/check type record
                  Exec sql
                    Select bf_fltunit, bf_fltdolr, bf_last30, bf_chktyp3
                      Into :bffltunit, :bffltdolr, :bflast30, :bfchktyp3
                      From k_buyrfil
                      Where bf_comp = :bfcomp and
                            bf_buyr = :bfbuyr and
                            bf_locn = :bflocn and
                            bf_chktyp3 = :bfchktyp3
                      Fetch first row only;

                  If SQLState = SQLStateOk;
                     BuyrFileNOT = *off;
                  else;
                     BuyrFileNOT = *on;
                  endif;
          endif;

       //-------------------------------------------------- Process products
       //process product record

         ptexceptn = 0;
         clear                 diff_days;
         clear                 diff_dolrs;
         filter = *off;

       //    get last 3 periods of demand
         clear                 per01;
         clear                 per02;
         clear                 per03;
         zz_histype =          *zeros;
         callp K3S_3700(pr_comp:
                        pr_locn:
                        pr_supl:
                        pr_suplsub:
                        pr_prod:
                        pr_forcint:
                        pr_forcyr:
                        pr_forcper:
                        zz_histype:
                        zz_hist01:
                        zz_hist02:
                        zz_hist03:
                        zz_hist04:
                        zz_hist05:
                        zz_hist06:
                        zz_hist07:
                        zz_hist08:
                        zz_hist09:
                        zz_hist10:
                        zz_hist11:
                        zz_hist12:
                        zz_hist13);
         per01 = zz_hist01;
         per02 = zz_hist02;
         per03 = zz_hist03;

       //    calculate difference between last period forecast and
       //    last period demand
         diff_units = per01 - pr_prvfore;

       //    get absolute value
         if       diff_units < 0;
                  diff_units *= -1;
         endif;
       //      convert to dollars
         diff_dolrs = diff_units * pr_costeac;

       //    keep difference in dollars from being too large
         if       diff_dolrs > 9999999.99;
                  diff_dolrs = 9999999.99;
         endif;

       //    exception type for units
         if       diff_units < bffltunit;
                  filter = *on;
                  ptexceptn = 1;
         endif;

       //    exception type for dollars
         if       filter = *off AND
                  diff_dolrs < bffltdolr;
                  filter = *on;
                  ptexceptn = 2;
         endif;

       //    exception type for days
         if       filter = *off AND
                  bflast30 = 1;
                  diff_days = %diff(lcsysdate:pr_formanl:*days);
                  if       diff_days <= 30;
                           filter = *on;
                           ptexceptn = 3;
                  else;
                           diff_days = 0;
                  endif;
         endif;

       //    no filter record set up for this product
         if  BuyrFileNOT = *on; //k_buyrfil not found
             filter = *off;
             ptexceptn = 0;
         endif;
       //    previous forecast was 0, so can not filter
         if       pr_prvfore = 0;
                  filter = *off;
                  ptexceptn = 0;
         endif;

       //filter period end exception
         if       filter = *on;
                  pr_endper = *blanks;
                  exsr updtprod;
         endif;

       //write product suggested order record
         exsr insert_prodflt;

         recs_written += 1;

         SQLState = SQLStateOk;
       enddo;

       exsr clsprcursor;
       //-------------------------------------------------- End of Main Loop

       //finished, set on LR
       *inlr = *on;

       begsr dclprcursor;
       exec sql
        declare prcursor cursor
         for
         select *
         from k_product
         where pr_comp = :comp and
               pr_endper <> ' ' and
               pr_deltcnt = 0 and
               pr_altsrce = 0
         order by pr_buyr,
                  pr_locn,
                  pr_endper;
       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor;
        if SQLState <> SQLStateOk;
           exsr clsprcursor;
           exec sql
            open prcursor;
        endif;
       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       begsr updtprod;
       Exec sql
         update k_product
           set pr_endper = :pr_endper
           where pr_comp = :comp and
                 pr_locn = :pr_locn and
                 pr_supl = :pr_supl and
                 pr_suplsub = :pr_suplsub and
                 pr_prod = :pr_prod;
       endsr;

       begsr insert_prodflt;
       Exec sql
         insert into k_prodflt
            (pt_comp,
             pt_buyr,
             pt_chktyp3,
             pt_locn,
             pt_fltunit,
             pt_fltdolr,
             pt_last30,
             pt_supl,
             pt_suplsub,
             pt_name,
             pt_prod,
             pt_desc1,
             pt_prodseq,
             pt_birth,
             pt_usrstat,
             pt_sysstat,
             pt_seasonl,
             pt_deltcnt,
             pt_per01,
             pt_per02,
             pt_per03,
             pt_prvfcst,
             pt_newfcst,
             pt_difunit,
             pt_difdolr,
             pt_difdays,
             pt_endper,
             pt_exceptn)
         values (:pr_comp,
                 :pr_buyr,
                 :bfchktyp3,
                 :pr_locn,
                 :bffltunit,
                 :bffltdolr,
                 :bflast30,
                 :pr_supl,
                 :pr_suplsub,
                 :spname,
                 :pr_prod,
                 :pr_desc1,
                 :pr_prodseq,
                 :lcsysdate,
                 :pr_usrstat,
                 :pr_sysstat,
                 :pr_seasonl,
                 :pr_deltcnt,
                 :per01,
                 :per02,
                 :per03,
                 :pr_prvfore,
                 :pr_forcast,
                 :diff_units,
                 :diff_dolrs,
                 :diff_days,
                 :pr_endpers,
                 :ptexceptn);
       endsr;
      /end-free
