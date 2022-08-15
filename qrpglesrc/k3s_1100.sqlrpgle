      *****************************************************************
     h copyright('(C) Copyright 1996 - 2015 King III Solutions, Inc.  +
     h Rel 5.2  2015-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)
     h OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2015 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_1100
      **   Type: ILE RPG Program
      **   Desc: Suggested order supplier approval PO header/detail
      **
      *****************************************************************
      **
      **  This program is used to create PO header and Detail records
      **  that is sent to customers PO system.
      **
      *****************************************************************
      **
      **  10/09/2014 King3 - When writing to K_SUPLLST, use current
      **                     supplier (and sub), to ensure Change
      **                     Product Source in effect.
      **
      *****************************************************************

     f*k_productauf   e           k disk                                         products
      * products by locn, supplier, sub supplier, prod

     f*_productluf   e           k disk    rename(rk_product:r1_product)        deal summary info
      * selected products batches

     f*_productcif   e           k disk    rename(rk_product:r2_product)        products
     f*                                    prefix(p2_:3)

     f*_companyaif   e           k disk                                         buy groups
      * Company values by company id

     f*_dealperbif   e           k disk                                         buy groups
      * Permanent deals file

     f*_suplsoqaif   e           k disk
      * supplier suggested order

     f*_prodsoqbuf   e           k disk
      * product suggested orders

     f*_intordbauf a e           k disk
      * approved order header

     f*_intorddauf a e           k disk
      * approved order detail

      * Supplier lost begin
     f*_supllst uf a e             disk                                         locations
      * supplier lost sales to alternate source
      * Supplier lost end


     f*_locatnsaif   e           k disk                                         locations
      * locations by location

     f*_prodhldbuf a e           k disk
      * product hold out quantities

     f*_tablcodaif   e           k disk                                         locations
      * table file

      **Log start
     f*_phldlogauf a e           k disk                                         deal summary info
      * product hold out changes log
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_1100        PI
     d  comp                          1
     d  buyr                          5
     d  locn                          5
     d  supl                         10
     d  suplsub                      10
     d  soqseq#                       5p 0
     d  pomessage1                   25
     d  pomessage2                   25
     d  pomessage3                   25
     d  pomessage4                   25
     d  pomessage5                   25
     d  pomessage6                   25
     d  pomessage7                   25
     d  popickup                      1
     d  podate                         d
     d  poarrive                       d
     d  poblank                      10
     d  potype                        1
     d  polocnfrm                     5
     d  polocnto                      5
     d  workstatn                    10
     d  hldbegin                       d
     d  hldend                         d
     d  hldreas                      15
     d  skip_lt                       1
     d  user_in                      10
     d  usera01                       1
     d  usera02                       1
     d  usera03                       1
     d  usera04                       1
     d  usera05                       5
     d  usera06                       5
     d  usera07                      10
     d  usera08                      10
     d  usera09                     100
     d  usera10                     100
     d
      * -------------------------------------------------------
     d Prod_DS         DS
     d  prqtyoord                          like(pr_qtyoord)
     d  prqtybaln                          like(pr_qtybaln)
     d  prlstordr                          like(pr_lstordr)
      * -------------------------------------------------------
     d Product_DS      DS
     d  p2altsrce                          like(pr_altsrce)
     d  p2tempory                          like(pr_tempory)
     d  p2buyr                             like(pr_buyr)
     d  p2supl                             like(pr_supl)
     d  p2suplsub                          like(pr_suplsub)
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * ------------------------------------ File information data structure
     d*copy k3s_c010
      *****************************************************************
      **
      **   K3S-Replenish - Inventory REPLENISHment System
      **   Copyright (c) 1996-1997 by King III Solutions, Inc.
      **   All rights reserved.
      **
      *****************************************************************
      **
      **   Name: K3S_C010
      **   Type: ILE /COPY member
      **   Desc: I/O feedback area
      **
      *****************************************************************

     d infds           ds
     d  wrk_statn            197    206a                                        work station id
     d  rec_format           261    270a                                        format being read
     d  key_press            369    369a                                        key being pressed
     d  pos_cursor           370    371b 0                                      key being pressed

     d  pos_row        s              3s 0                                      key being pressed
     d  pos_column     s              3s 0                                      key being pressed

     d  alp_row        s              3                                         key being pressed
     d  alp_column     s              3                                         key being pressed
     d  #locn          s                   like(so_locn)

      * ---------------------------------------------------- Local Data Area
     d*copy k3s_c030
      *****************************************************************
      **
      **   K3S-Replenish - Inventory REPLENISHment System
      **   Copyright (c) 1996-1997 by King III Solutions, Inc.
      **   All rights reserved.
      **
      *****************************************************************
      **
      **   Name: K3S_C030
      **   Type: ILE /COPY member
      **   Desc: *LDA Local data area assignments
      **
      *****************************************************************

     d                 ds                  dtaara(*lda)
     d lda_dummy1                   256                                         spacing for now 1
     d lda_dummy2                   256                                         spacing for now 2
     d lda_userid                    10                                         user id
     d lda_comp                       1                                         company ID
     d lda_compcd                     3                                         company code
     d lda_usrdat                     4                                         user date format
     d lda_usrtim                     4                                         user time format
     d lda_usradj                     3p 0                                      user time adj. hours
     d lda_sysdat                      d   datfmt(*iso)                         system date (*ISO)
     d lda_cmpdat                      d   datfmt(*iso)                         computer date (*ISO)
     d lda_alarm                      1p 0                                      alarm flag
     d lda_dealex                     3p 0                                      deal expire # days
     d lda_dealal                     1                                         deal allowance $,D,%
     d lda_dealpi                     1                                         deal pri incrs $,D,%
     d lda_date6                      1p 0                                      date entry 6 digit
     d lda_domain                    10                                         user domain
     d lda_managr                     1p 0                                      manager flag
     d lda_buyr                       5                                         buy group default
     d lda_window                     1p 0                                      if error, dsp window
     d lda_locn                       5                                         location default
     d lda_orview                     1p 0                                      order summary view
     d lda_ormode                     1                                         order summary mode
     d lda_prview                     1p 0                                      product summary view
     d lda_proptn                     1p 0                                      product summary optn
     d lda_hisdsp                     1p 0                                      product hist typ dsp
     d lda_trcksg                     3p 3                                      product t/s > disply
     d lda_trmprd                     1p 0                                      trim product ID type
     d lda_untdsp                     1p 0                                      unit display 1-6
     d lda_selchk                     2                                         select check
     d lda_prdsum                     1p 0                                      reverse image flag
     d lda_mltwrn                     1p 0                                      soq mult warning flg
     d lda_altvu                      1p 0                                      alt sou availty view
     d lda_logwrn                     3p 0                                      F19=Log warning days
     d lda_exnote                     3p 0                                      expire notes days
     d lda_severe                     1p 0                                      severe errors flag
     d lda_f3_wrn                     1p 0                                      F3=Exit warning
     d lda_cr1010                     1p 0                                      cursor pos k3s_1010
     d lda_cstwrn                     1p 0                                      cost/div chg warning
     d lda_arvdat                     1p 0                                      arrival date type
     d lda_autrnd                     1p 0                                      auto rounding flag
     d lda_autfrz                     1p 0                                      auto freeze flag
     d lda_frzdat                      d   datfmt(*iso)                         freeze date (*ISO)
     d lda_noteky                     1p 0                                      supl in prod note ky
     d lda_slview                     1p 0                                      order summary view
     d lda_cnv                       10                                         order summary view
     d lda_dly                       10                                         order summary view
     d lda_dta                       10                                         order summary view
     d lda_mod                       10                                         order summary view
     d lda_obj                       10                                         order summary view
     d lda_qry                       10                                         order summary view

      * --------------------------------------------------- parameter passed
     d transacum       s              1                                         supplier
     d vldatng         s              3  3                                      value dating %
     d mode            s              1  0                                      Mode=1 from K3S_1500
     d disc_accum      s             11  4                                      accum disc eaches
     d dealwindow      s                   like(pr_dealbeg)
     d deal_days       s              3  0                                      extra days to buy
     d po_done         s              1a                                        company
     d pgm             s             10a                                        returned date frmtd
     d soqseq          s              5a                                        soq sequence #
     d poapprove       s              1a                                        save buy group id
     d xx_costeac      s                   like(pq_costeac)                     save buy group id
     d actunet         s                   like(so_actunet)                     save buy group id
     d disc_each       s             11  4                                      Discount value each
     d pdlocn          s              5                                         location
     d pdsuplorg       s             10                                         supplier
     d pdsuplors       s             10                                         sub supplier
     d pdprod          s             25                                         soq sequence #
     d file            s             10                                         soq sequence #
     d libr            s             10                                         soq sequence #
     d altsrce         s              1                                         soq sequence #
     d location        s              5                                         soq sequence #
     d supplier        s             10                                         soq sequence #
     d suplsb          s             10                                         soq sequence #
     d taflag1         s              1  0
     d cmapprove       s              1  0
     d lcsysdate       s               d
     d tacodeds1       s            100
     d #supl           s                   like(pu_supl)
     d #suplsub        s                   like(pu_suplsub)
     d #plocn          s                   like(pq_locn)
     d*
     d #drcomp         s                   like(pq_comp)
     d #drlocn         s                   like(pq_locn)
     d #drsupl         s                   like(pr_supl)
     d #drsuplsub      s                   like(pr_suplsub)
     d #drprod         s                   like(pr_prod)

     d time_stamp      s               z   inz                                  send start point lo
     d time            s               t   timfmt(*iso)                         send start point lo
     d date            s               d   datfmt(*iso)                         send start point lo

      * -------------------------------------- Program Status Data Structure
     d*copy k3s_c040
      *****************************************************************
      **
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
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d dealper_rec   e ds                  ExtName(k_dealper)
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
     d intordb_rec   e ds                  ExtName(k_intordb)
     d product_rec   e ds                  ExtName(k_product)
     d intordd_rec   e ds                  ExtName(k_intordd)
     d supllst_rec   e ds                  ExtName(k_supllst)
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
     d prodhld_rec   e ds                  ExtName(k_prodhld)
     d phldlog_rec   e ds                  ExtName(k_phldlog)
     d trancen_rec   e ds                  ExtName(k_trancen)
      * -------------------------------------- Likerec Statements
      /free
       //Log end
       //--------------------------------------------------------- main line
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclprcursor;
       exsr dclprcursorcm;
       exsr dclp2cursor;
       //get header information
       exsr $_get_lda;
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
           where ta_comp = :lda_comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'K3S_1100  POTYPE_T  '
           fetch first row only;
       transacum = *off;

       //get po transfer accumulate
       if SQLState = SQLStateOk;
          if taflag1 = 1;
             transacum = *on;
          else;
             transacum = *off;
          endif;
       else;
          transacum = *off;
       endif;

       exec sql
         select *
           into :dealper_rec
           from k_dealper
           where dr_comp = :lda_comp
           fetch first row only;

       if SQLState = SQLStateOk;
          file = 'IN USE    ';
       else;
          file = '          ';
       endif;

       exec sql
         select cm_approve
           into :cmapprove
           from k_company
           where cm_comp = :lda_comp
           fetch first row only;

       exec sql
         select lc_sysdate
           into :lcsysdate
           from k_locatns
           where lc_comp = :lda_comp and
                 lc_locn = :locn
           fetch first row only;

       exec sql
         select *
           into :suplsoq_rec
           from k_suplsoq
           where so_comp = :comp and
                 so_buyr = :buyr and
                 so_locn = :locn and
                 so_supl = :supl and
                 so_suplsub = :suplsub and
                 so_soqseq# = :soqseq#
           fetch first row only;

       if SQLState = SQLStateOk;
          ib_comp = so_comp;
          if so_suplsub <> 'h';
             ib_locn = so_locn;
             #locn = so_locn;
          endif;
          if so_suplsub = 'h';
             ib_locn = so_cmblocn;
             #locn = so_cmblocn;
          endif;
       //No split
          if so_suplsub = 'h' and
             so_spl1typ = *blanks;
                ib_locn = so_locn;
                #locn = so_locn;
          endif;
       //End no split
          ib_po# = so_po#;
          ib_locn = #locn;
          if potype = 'T';
             ib_locnfrm = polocnfrm;
             ib_locnto  = polocnto;
          else;
             ib_locnfrm = *blanks;
             ib_locnto  = *blanks;
          endif;

          if (potype = 'T')         and
             (workstatn = 'ONLINE') and
             (ib_locnfrm = *blanks) and
             (ib_locnto = *blanks);

              exec sql
                select *
                  into :trancen_rec
                  from k_trancen
                  where tc_comp = :comp and
                        tc_locnto = :locn and
                        tc_supl = :supl and
                        tc_suplsub = :suplsub
                  fetch first row only;
              if SQLState = SQLStateOk;
                 ib_locnto = tc_locnto;
                 ib_locnfrm = tc_locnfrm;
              endif;
          endif;

          exec sql
            select *
              into :intordb_rec
              from k_intordb
              where ib_comp = :so_comp and
                    ib_locn = :#locn and
                    ib_po# = :so_po#
              fetch first row only;
          if SQLState = RowNotFound;
             po_done = 'Y';
             ib_buyr = so_buyr;
             ib_birth = lda_cmpdat;
             clear ib_lastupd;
             if so_suplsub <> 'h';
                ib_supl    = supl;
                ib_suplsub = suplsub;
             endif;
            if so_suplsub = 'h';
               ib_supl    = so_cmbsupl;
               ib_suplsub = so_cmbsub;
            endif;
       //No split
            if so_suplsub = 'h' and
               so_spl1typ = *blanks;
                  ib_supl    = so_supl;
                  ib_suplsub = so_suplsub;
            endif;
       //End no split
            ib_pickup = %dec(popickup:1:0);
            ib_podate = podate;
            ib_arrive = poarrive;
            ib_coment1 = pomessage1;
            ib_coment2 = pomessage2;
            ib_coment3 = pomessage3;
            ib_coment4 = pomessage4;
            ib_coment5 = pomessage5;
            ib_coment6 = pomessage6;
            ib_coment7 = pomessage7;
            ib_blank = poblank;
       //
            ib_po_flag = 0;
       //
            ib_potype = potype;
            if so_suplsub <> 'h';
               ib_altsrce = so_altsrce;
            endif;
            if so_suplsub = 'h';
               ib_altsrce = 0;
            endif;

            if skip_lt = 'N' or '0';
               ib_skip_lt = 0;
            else;
               ib_skip_lt = 1;
            endif;

            callp K3S_Retrieve_Timestamp(time_stamp);
            time = %time(time_stamp);
            date = %date(%subst(%char(time_stamp):1:10):*ISO);
            ib_timestp = time_stamp;
            if user_in <> *blanks;
               ib_user = user_in;
            else;
               ib_user    = psds_user;
            endif;
            ib_workstn = workstatn;

            if ib_workstn = 'AUTO_PO   ';

               exec sql
                 select ta_codeds1
                   into :tacodeds1
                   from k_tablcod
                   where ta_comp = :lda_comp and
                         ta_codetyp = 'BUY' and
                         ta_codeval = :ib_buyr
                   fetch first row only;
               ib_user = tacodeds1;
            endif;

            exsr insert_intordb;
          endif;
       endif;

       //---------------------------------------------------- start products
       //start with first product for this supplier for suggested order
       if cmapprove = 0;
          exsr dclpqcursor;
          exsr opnpqcursor;

          actunet = *zeros;

       //loop through all products in this order
          dow SQLState = SQLStateOk;
       //read products in order
             exec sql
               fetch next
                 from pqcursor
                 into :prodsoq_rec;
       //only process when record read
             if SQLState = SQLStateOk and
                pq_soqact > 0;
       //regular supplier
                if pq_suplsub <> 'h';
                   #plocn = pq_locn;
                endif;
       //combination supplier
                if pq_suplsub = 'h';
                   #plocn = pq_cmblocn;
                endif;

                exsr InzInpSrch;
    ‚   //initialize StmtString
    ‚            exsr IntSQLStmt;
    ‚   //prepare statement
    ‚            exsr PrepDynSQLStmt;

                if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                   exsr opnprcursor;

                   if SQLState = SQLStateOk;
                      exec sql
                        fetch next
                          from prcursor
                          into :product_rec;

                          if SQLState = SQLStateOk;
                             id_costreg = pr_costreg;
                          else;
                             id_costreg = 0;
                          endif;
                   endif;
                endif;
                id_comp = pq_comp;
                if pq_suplsub <> 'h';
                   id_locn = pq_locn;
                   #locn = pq_locn;
                endif;
                if pq_suplsub = 'h';
                   id_locn = pq_cmblocn;
                   #locn = pq_cmblocn;
                endif;
       //No split
                if pq_suplsub = 'h' and
                   so_spl1typ = *blanks;
                      id_locn = pq_locn;
                      #locn = pq_locn;
                endif;
       //End no split
                id_po#  = so_po#;
                id_prod = pq_prod;
                exec sql
                  select *
                    into :intordd_rec
                    from k_intordd
                    where id_comp = :pq_comp and
                          id_locn = :#locn and
                          id_po# = :so_po# and
                          id_prod = :pq_prod
                    fetch first row only;
                if SQLState = RowNotFound;
                   id_birth = lda_cmpdat;
                   clear id_lastupd;
                   exsr $_prodseq;

                   id_deal = *blanks;
                   if (pr_dealuse < pr_dealalw) and
                      (pr_deal <> *blanks) and
                      (so_altsrce <> 1) and
                      (potype <> 'T') and
                      (pr_dealalw > *zeros) or
                      (pr_deal <> *blanks) and
                      (so_altsrce <> 1) and
                      (potype <> 'T') and
                      (pr_dealalw = *zeros);

       //process deal information

       //   A deal does exist for this product,
                      if (pr_deal    <> *blanks) and
                        (pr_dealbeg <= lcsysdate) and
                        (pr_dealend >= lcsysdate);
                        if pr_dealuse < 900;
                           pr_dealuse += 1;
                        endif;
                        id_deal    = pq_deal;
                      endif;
                   endif;

                   if pq_suplsub <> 'h';
                      id_altsrce = pq_altsrce;
                   endif;
                   if pq_suplsub = 'h';
                      id_altsrce = 0;
                   endif;
                   id_costord = pq_costord;
                   id_costeac = pq_costeac;
                   id_costdiv = pq_costdiv;
                   id_soqact  = pq_soqact;
                   id_soqacto = pq_soqact;
                   if file = 'IN USE    ' and
                      so_altsrce <> 1;
                         exsr $_prm_deal;
                   endif;

       // regular orders
                   if id_altsrce = 0;
                      pr_lstordr = lda_cmpdat;
       //9999 stuf
                      if pr_qtyoord + pq_soqact > 9999999;
                         pr_qtyoord = 9999999;
                      else;
                         pr_qtyoord += pq_soqact;
                      endif;
                      if pr_qtybaln + pq_soqact > 9999999;
                         pr_qtybaln = 9999999;
                      else;
                         pr_qtybaln += pq_soqact;
                      endif;
       //end 9999 stuff

       //     mark forward bought products with future date
                      if pq_fbxdays > 0;
                         pr_fbuydat = lda_cmpdat + %days(pq_fbxdays);
                      endif;

                   else;
       // alternate source orders
       //9999 stuff
                     if pr_altoord + pq_soqact > 9999999;
                        pr_altoord = 9999999;
                     else;
                        pr_altoord += pq_soqact;
                     endif;
       //end 9999 stuff
                   endif;

       //9999 stuff
                   if pq_qtybaln + pq_soqact > 9999999;
                      pq_qtybaln = 9999999;
                   else;
                      pq_qtybaln += pq_soqact;
                   endif;
                   if pq_qtyoord + pq_soqact > 9999999;
                      pq_qtyoord = 9999999;
                   else;
                      pq_qtyoord += pq_soqact;
                   endif;
        //end 9999 stuff

        //    if buyer provided hold out reason, place in file
                   if hldreas <> *blanks;
                      pu_supl    = supl;
                      pu_suplsub = suplsub;
                      #supl      = supl;
                      #suplsub   = suplsub;

                      if pu_supl    <> pr_suplorg OR
                         pu_suplsub <> pr_suplors;

                         pu_supl    = pr_suplorg;
                         pu_suplsub = pr_suplors;
                         #supl      = pr_suplorg;
                         #suplsub   = pr_suplors;
                         exec sql
                           select *
                             into :prodhld_rec
                             from k_prodhld
                             where pu_comp = :lda_comp and
                                   pu_locn = :locn and
                                   pu_supl = :#supl and
                                   pu_suplsub = :#suplsub and
                                   pu_prod = :id_prod and
                                   pu_timestp = :time_stamp
                             fetch first row only;
                      endif;
                      if SQLState = RowNotFound;
                         pu_birth = lda_cmpdat;
                         pu_lastupd = lda_cmpdat;
                         pu_prod = id_prod;
                         pu_po#  = so_po#;
                         pu_hldqty = pq_soqact;
                         pu_begin  = hldbegin;
                         pu_end    = hldend;
                         pu_hldreas = hldreas;
                         pu_comp    = lda_comp;
                         pu_locn    = locn;
       //***             eval      pu_supl    = supl
       //**              pu_suplsub = suplsub;
                         pu_suplusr = pr_suplusr;
                         pu_suplusb = pr_suplusb;
                         pu_timestp = time_stamp;
                         exsr insert_prodhld;
        //Log start
                        exsr      $_add_3056;
        //Log end
                      endif;
                   endif;

        //Supplier lost begin
                   if pq_altsrce = 1;
                      exsr InzInpSrchp2;
    ‚   //initialize StmtString
    ‚                  exsr IntSQLStmtp2;
    ‚   //prepare statement
    ‚                  exsr PrepDynSQLStmtp2;

                      if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                         exsr opnp2cursor;

                         Dow SQLState = SQLStateOk;
                           exec sql
                             fetch next
                                from p2cursor
                                into :product_ds;
                            if SQLState = RowNotFound;
                               leave;
                            endif;
                            if p2altsrce = 0 and
                               p2tempory = 0;
                                  leave;
                              endif;
                         enddo;
                         st_comp = pq_comp;
                         st_birth = pq_birth;
                         st_podate  = ib_podate;
                         st_po#     = id_po#;
                         st_buyr    = p2buyr;
                         st_locn    = pq_locn;
                         st_supl    = pq_supl;
                         st_suplsub = pq_suplsub;
                         st_suplusr = pq_suplusr;
                         st_suplusb = pq_suplsub;
                         st_prod    = pq_prod;
                         st_soqact  = pq_soqact;
                         st_costord = pq_costord;
                         st_costdiv = pq_costdiv;
                         st_disothr = pq_disothr;
                         exsr insert_supllst;
                      exsr clsp2cursor;
                   endif;

       //Supplier lost end

                   exsr insert_intordd;
                   exsr updtproduct;

                   exsr clsprcursor;

                   exsr updt_prodsoq;

                   if pq_suplsub = 'h';

                      exsr InzInpSrchcm;
    ‚   //initialize StmtString
    ‚                  exsr IntSQLStmtcm;
    ‚   //prepare statement
    ‚                  exsr PrepDynSQLStmtcm;

                     if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                        exsr opnprcursorcm;

                        if SQLState = SQLStateOk;
                           exec sql
                             fetch next
                               from prcursorcm
                               into :prod_ds;

                           if SQLState = SQLStateOk;
                              prlstordr = lda_cmpdat;
       //9999 stuff
                              if prqtyoord + pq_soqact > 9999999;
                                 prqtyoord = 9999999;
                              else;
                                 prqtyoord += pq_soqact;
                              endif;
                              if prqtybaln + pq_soqact > 9999999;
                                 prqtybaln = 9999999;
                              else;
                                 prqtybaln += pq_soqact;
                              endif;
       //end 9999 stuff
                              exsr updtproductcm;
                              exsr clsprcursorcm;
                           endif;
                        endif;
                     endif;
                   endif;

                   if potype = 'T';
                      exsr InzInpSrchp2;
    ‚   //initialize StmtString
    ‚                  exsr IntSQLStmtp2;
    ‚   //prepare statement
    ‚                  exsr PrepDynSQLStmtp2;

                      if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                         exsr opnp2cursor2;

                         Dow SQLState = SQLStateOk;
                           exec sql
                             fetch next
                                from p2cursor
                                into :product_ds;
                            if SQLState = SQLStateOk and
                               p2altsrce = 0 and
                               p2tempory = 0;
                                  leave;
                              endif;
                         enddo;
                         exsr clsp2cursor;
       //position to correct product record for Hub location

                         exsr InzInpSrch2;
    ‚   //initialize StmtString
    ‚                     exsr IntSQLStmt;
    ‚   //prepare statement
    ‚                     exsr PrepDynSQLStmt;

                         if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                            exsr opnprcursor;

                            if SQLState = SQLStateOk;
                               exec sql
                                  fetch next
                                    from prcursor
                                    into :product_rec;

                               if SQLState = SQLStateOk;
                                  if transacum = *on;
       //9999 stuff
                                     if pr_accdem + pq_soqact > 9999999;
                                        pr_accdem = 9999999;
                                     else;
                                        pr_accdem += pq_soqact;
                                     endif;
                                     if pr_accsale + pq_soqact > 9999999;
                                        pr_accsale = 9999999;
                                     else;
                                        pr_accsale += pq_soqact;
                                     endif;
                                     if pr_tranptd + pq_soqact > 9999999;
                                        pr_tranptd = 9999999;
                                     else;
                                        pr_tranptd += pq_soqact;
                                     endif;
       //end 9999 stuff
                                  endif;
                                  exsr updtproduct2;
                                  exsr clsprcursor;
                               endif;
                            endif;
                         endif;
                      endif;
                   endif;
                endif;
             endif;
             endif;
          enddo;
          exsr clspqcursor;

          if po_done = 'Y';
             if so_suplsub <> 'h';
                altsrce = %editc(so_altsrce:'X');
             endif;
             if so_suplsub = 'h';
                altsrce = '0';
             endif;
             soqseq = %editc(so_soqseq#:'X');
             if so_suplsub <> 'h';
                location     = so_locn;
                supplier     = so_supl;
                suplsb       = so_suplsub;
                poapprove    = 'A';
                callp K3S_1101CL(so_comp:
                                 location:
                                 so_po#:
                                 so_buyr:
                                 supplier:
                                 suplsb:
                                 soqseq:
                                 altsrce:
                                 poapprove:
                                 ib_potype:
                                 usera01:
                                 usera02:
                                 usera03:
                                 usera04:
                                 usera05:
                                 usera06:
                                 usera07:
                                 usera08:
                                 usera09:
                                 usera10);
             endif;
             if so_suplsub = 'h';
                location     = so_cmblocn;
                supplier     = so_cmbsupl;
                suplsb       = so_cmbsub;
                poapprove = 'A';
                callp K3S_1101CL(so_comp:
                                 location:
                                 so_po#:
                                 so_buyr:
                                 supplier:
                                 suplsb:
                                 soqseq:
                                 altsrce:
                                 poapprove:
                                 ib_potype:
                                 usera01:
                                 usera02:
                                 usera03:
                                 usera04:
                                 usera05:
                                 usera06:
                                 usera07:
                                 usera08:
                                 usera09:
                                 usera10);
             endif;
          endif;

         endif;
       //------------------------------------------------------ end products

       *inlr = *on;

       //-------------------------------------------------get lda subroutine
       //copy k3s_c031
       //**************************************************************
       //
       //  K3S-Replenish - Inventory REPLENISHment System
       //  Copyright (c) 1996-1997 by King III Solutions, Inc.
       //  All rights reserved.
       //
       //**************************************************************
       //
       //  Name: K3S_C031
       //  Type: ILE /COPY member
       //  Desc: *LDA Local data area - get LDA data
       //
       //**************************************************************
       ////////////////////////////////////////////////// Get data area *lda

       begsr $_get_lda;

       //retrieve local data area *lda
       in *dtaara;

       //if K3S-Replenish date not equal AS/400 system date, set on *in95
       //  to display date with underline attribute
       *in95 = (lda_sysdat <> lda_cmpdat);

       //if user preferred time adjustment being used, set on indicator 96
       //  to display time with underline attribute
       *in96 = (lda_usradj <> 0);

       endsr;


       //-------------------------------------------- build product sequence
       //copy k3s_c240
       //py k3s_c240
       //**************************************************************
       //
       //  K3S-Replenish - Inventory REPLENISHment System
       //  Copyright (c) 1996-1997 by King III Solutions, Inc.
       //  All rights reserved.
       //
       //**************************************************************
       //
       //  Name: K3S_C240
       //  Type: ILE /COPY member
       //  Desc: Build product sequence for field id_prodseq in file
       //        k_intordd. Used in programs K3S_1100 and K3S_1110
       //
       //**************************************************************
       //
       // customization of the ID_PRODSEQ field can take place in this
       // subroutine.
       //
       // Default will just be PQ_PRODSEQ

       ////////////////////////////////////////////// build field id_prodseq

       begsr $_prodseq;

       select;

       //  narcotics
         when id_prod <= '009999';
            id_prodseq = '1' +
            pq_suplorn +
            pq_suplorg +
            pq_suplors +
            pq_prodseq;

       //  controled products
         when id_prod <= '099999';
            id_prodseq = '2' +
            pq_suplorn +
            pq_suplorg +
            pq_suplors +
            pq_prodseq;

       //  all other products
         other;
            id_prodseq = '3' +
            pq_suplorn +
            pq_suplorg +
            pq_suplors +
            pq_prodseq;

       endsl;

       id_prodseq = pq_prodseq;

       endsr;


       ////////////////////////////////////////////////////// Get prm deals

       begsr $_prm_deal;

       exsr dcldrcursor;
       //select type of L1
       #drcomp = lda_comp;
       #drlocn = pq_locn;
       #drsupl = *blanks;
       #drsuplsub = *blanks;
       #drprod = *blanks;

       exsr InzInpSrchdr;
    ‚   //initialize StmtString
    ‚   exsr IntSQLStmtdr;
    ‚   //prepare statement
    ‚   exsr PrepDynSQLStmtdr;

       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
          exsr opndrcursor;

          dow SQLState = SQLStateOk;
       //read products in order
             exec sql
               fetch next
                 from drcursor
                 into :dealper_rec;

                 if SQLState = RowNotFound;
                    leave;
                 endif;

                 exsr $_cal_deal;
          enddo;
          exsr clsdrcursor;
       endif;

       //        select type of P1
       #drcomp = lda_comp;
       #drlocn = *blanks;
       #drsupl = pq_suplorg;
       #drsuplsub = pq_suplors;
       #drprod = pq_prod;

       exsr InzInpSrchdr;
    ‚   //initialize StmtString
    ‚   exsr IntSQLStmtdr;
    ‚   //prepare statement
    ‚   exsr PrepDynSQLStmtdr;

       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
          exsr opndrcursor;

          dow SQLState = SQLStateOk;
       //read products in order
             exec sql
               fetch next
                 from drcursor
                 into :dealper_rec;

                 if SQLState = RowNotFound;
                    leave;
                 endif;

                 exsr $_cal_deal;
          enddo;
          exsr clsdrcursor;
       endif;

       //        select type of P2
       #drcomp = lda_comp;
       #drlocn = pq_locn;
       #drsupl = pq_suplorg;
       #drsuplsub = pq_suplors;
       #drprod = pq_prod;

       exsr InzInpSrchdr;
    ‚   //initialize StmtString
    ‚   exsr IntSQLStmtdr;
    ‚   //prepare statement
    ‚   exsr PrepDynSQLStmtdr;

       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
          exsr opndrcursor;

          dow SQLState = SQLStateOk;
       //read products in order
             exec sql
               fetch next
                 from drcursor
                 into :dealper_rec;

                 if SQLState = RowNotFound;
                    leave;
                 endif;

                 exsr $_cal_deal;
          enddo;
          exsr clsdrcursor;
       endif;
       //        select type of S1
       #drcomp = lda_comp;
       #drlocn = *blanks;
       #drsupl = pq_suplorg;
       #drsuplsub = pq_suplors;
       #drprod = *blanks;

       exsr InzInpSrchdr;
    ‚   //initialize StmtString
    ‚   exsr IntSQLStmtdr;
    ‚   //prepare statement
    ‚   exsr PrepDynSQLStmtdr;

       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
          exsr opndrcursor;

          dow SQLState = SQLStateOk;
       //read products in order
             exec sql
               fetch next
                 from drcursor
                 into :dealper_rec;

                 if SQLState = RowNotFound;
                    leave;
                 endif;

                 exsr $_cal_deal;
          enddo;
          exsr clsdrcursor;
       endif;

       //        select type of S2
       #drcomp = lda_comp;
       #drlocn = pq_locn;
       #drsupl = pq_suplorg;
       #drsuplsub = pq_suplors;
       #drprod = *blanks;

       exsr InzInpSrchdr;
    ‚   //initialize StmtString
    ‚   exsr IntSQLStmtdr;
    ‚   //prepare statement
    ‚   exsr PrepDynSQLStmtdr;

       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
          exsr opndrcursor;

          dow SQLState = SQLStateOk;
       //read products in order
             exec sql
               fetch next
                 from drcursor
                 into :dealper_rec;

                 if SQLState = RowNotFound;
                    leave;
                 endif;

                 exsr $_cal_deal;
          enddo;
          exsr clsdrcursor;
       endif;

       endsr;

       //////////////////////////////////////////////// Calculate prm deals

       begsr $_cal_deal;

       if dr_apply = 'O';

       //--------------------------------------------------------- discounts
       //discounts

       //        select type of discount
          select;

       //   *****************************
       //   percent discount
          when dr_facttyp = '%';
             eval(h)   disc_each = pr_costeac * dr_factval / 100;
             id_costeac -= disc_each;
             eval(h)   disc_each = pr_costreg * dr_factval / 100;
             id_costord -= disc_each;


       //   *****************************
       //   actual dollar value
          when dr_facttyp = '$';
             id_costeac = (dr_factval/dr_unit);
             id_costord = dr_factval;

       //   *****************************
       //   dollar difference
          when dr_facttyp = 'D';

             if        (dr_factval/dr_unit) < pr_costeac;
                       eval(h)   disc_each = dr_factval / dr_unit;
                       id_costeac -= disc_each;
                       id_costord -= dr_factval;
             endif;

          endsl;

       endif;

       endsr;

       //Log start
       ///////////////////////////////////////////////// Add user

       begsr $_add_3056;

       callp K3S_Retrieve_Timestamp(time_stamp);
       time = %time(time_stamp);
       date = %date(%subst(%char(time_stamp):1:10):*ISO);

       exec sql
         Select *
           into :phldlog_rec
           from k_phldlog
           where h0_comp = :lda_comp and
                 h0_locn = :pu_locn and
                 h0_supl = :pu_supl and
                 h0_suplsub = :pu_suplsub and
                 h0_prod = :pu_prod and
                 h0_birth = :date and
                 h0_birthtm = :time
           fetch first row only;

           if SQLState = RowNotFound;
              exsr insert_phldlog;
           endif;

       endsr;

       begsr insert_phldlog;
       exec sql
          insert into k_phldlog
            (h0_comp,
             h0_chgtype,
             h0_birth,
             h0_birthtm,
             h0_user,
             h0_workstn,
             h0_program,
             h0_locn,
             h0_supl,
             h0_suplsub,
             h0_begin,
             h0_end,
             h0_hldreas,
             h0_prod,
             h0_hldqty,
             h0_po#)
          values (:lda_comp,
                  'A',
                  :date,
                  :time,
                  :ib_user,
                  :workstatn,
                  :psds_progm,
                  :pu_locn,
                  :pu_supl,
                  :pu_suplsub,
                  :pu_begin,
                  :pu_end,
                  :pu_hldreas,
                  :pu_prod,
                  :pu_hldqty,
                  :pu_po#);
       endsr;

       //Log end
       begsr insert_intordb;
         Exec sql
           insert into k_intordb
           values (:intordb_rec);
       endsr;

       begsr insert_prodhld;
         Exec sql
           insert into k_prodhld
           values (:prodhld_rec);
       endsr;

       begsr insert_intordd;
         Exec sql
           insert into k_intordd
           values (:intordd_rec);
       endsr;

       begsr insert_supllst;
         Exec sql
           insert into k_supllst
           values (:supllst_rec);
       endsr;

       begsr dclpqcursor;
       exec sql
        declare pqcursor Cursor
          for
        select *
          from k_prodsoq
          where pq_comp = :comp and
                pq_buyr = :buyr and
                pq_locn = :locn and
                pq_supl = :supl and
                pq_suplsub = :suplsub and
                pq_soqseq# = :soqseq#
          order by pq_comp,
                   pq_buyr,
                   pq_locn,
                   pq_supl,
                   pq_suplsub,
                   pq_soqseq#
          for update of pq_qtybaln,
                        pq_qtyoord;
       endsr;

       begsr opnpqcursor;
       exec sql
        open pqcursor;
       endsr;

       begsr clspqcursor;
       exec sql
        close pqcursor;
       endsr;

       begsr updt_prodsoq;
         Exec sql
           update k_prodsoq
             set pq_qtybaln = :pq_qtybaln,
                 pq_qtyoord = :pq_qtyoord
             where current of pqcursor;
       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor
          using :lda_comp,
                :#plocn,
                :pq_suplorg,
                :pq_suplors,
                :pq_prod;
       endsr;

       begsr updtproduct;
       exec sql
        update k_product
          set pr_altoord = :pr_altoord,
              pr_qtybaln = :pr_qtybaln,
              pr_lstordr = :pr_lstordr,
              pr_qtyoord = :pr_qtyoord,
              pr_dealuse = :pr_dealuse,
              pr_fbuydat = :pr_fbuydat
          where current of pr_cursor;
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Product +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pr_comp = ? and +
                     pr_locn = ? and +
                     pr_supl = ? and +
                     pr_suplsub = ? and +
                     pr_prod = ? +
                     Order by pr_comp, +
                              pr_locn, +
                              pr_supl, +
                              pr_suplsub, +
                              pr_prod +
                     For update of pr_altoord, +
                                   pr_qtybaln, +
                                   pr_lstordr, +
                                   pr_dealuse, +
                                   pr_qtyoord, +
                                   pr_fbuydat';
       endsr;

       Begsr InzInpSrch2;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pr_comp = ? and +
                     pr_locn = ? and +
                     pr_supl = ? and +
                     pr_suplsub = ? and +
                     pr_prod = ? +
                     Order by pr_comp, +
                              pr_locn, +
                              pr_supl, +
                              pr_suplsub, +
                              pr_prod +
                     For update of pr_accdem, +
                                   pr_accsale, +
                                   pr_tranptd';
       endsr;

       begsr dclprcursor;
       exec sql
        declare prcursor Cursor
         for DynSQLStmt;
       endsr;

       begsr dclprcursorcm;
       exec sql
        declare prcursorcm Cursor
         for DynSQLStmtcm;
       endsr;

       Begsr InzInpSrchcm;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pr_comp = ? and +
                     pr_locn = ? and +
                     pr_supl = ? and +
                     pr_suplsub = ? and +
                     pr_prod = ? and +
                     pr_cmblocn = ? and +
                     pr_cmbsupl = ? and +
                     pr_cmbsub = ? +
                     Order by pr_comp, +
                              pr_locn, +
                              pr_supl, +
                              pr_suplsub, +
                              pr_prod, +
                              pr_cmblocn, +
                              pr_cmbsupl, +
                              pr_cmbsub +
                     For update of pr_lstordr, +
                                   pr_qtyoord, +
                                   pr_qtybaln';
       endsr;

       begsr opnprcursorcm;
       exec sql
        open prcursorcm
          using :lda_comp,
                :pq_locn,
                :pq_supl,
                :pq_suplsub,
                :pq_prod,
                :pq_cmblocn,
                :pq_cmbsupl,
                :pq_cmbsub;
       endsr;

       begsr updtproductcm;
       exec sql
        update k_product
          set pr_qtyoord = :prqtyoord,
              pr_qtybaln = :prqtybaln,
              pr_lstordr = :prlstordr
          where current of pr_cursorcm;
       endsr;

       begsr PrepDynSQLStmtcm;
       exec sql
        Prepare DynSqlStmtcm
          From :StmtString;
       endsr;

       Begsr IntSQLStmtcm;
       String = *blanks;
       String =   'Select pr_qtyoord, pr_qtybaln, pr_lstordr +
                   From K_product +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr opnprcursor2;
       exec sql
        open prcursor
          using :lda_comp,
                :polocnfrm,
                :p2supl,
                :p2suplsub,
                :pq_prod;
       endsr;


       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       begsr clsprcursor2;
       exec sql
        close prcursor2;
       endsr;

       begsr clsprcursorcm;
       exec sql
        close prcursorcm;
       endsr;

       begsr updtproduct2;
       exec sql
        update k_product
          set pr_accdem = :pr_accdem,
              pr_accsale = :pr_accsale,
              pr_tranptd = :pr_tranptd
          where current of pr_cursor;
       endsr;

       begsr dcldrcursor;
       exec sql
        declare drcursor Cursor
           for DynSQLStmtdr;
       endsr;

       Begsr InzInpSrchdr;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'dr_comp = ? and +
                     dr_locn = ? and +
                     dr_supl = ? and +
                     dr_suplsub = ? and +
                     dr_prod = ? +
                     Order by dr_comp, +
                              dr_locn, +
                              dr_supl, +
                              dr_suplsub, +
                              pr_prod';
       endsr;

       begsr PrepDynSQLStmtdr;
       exec sql
        Prepare DynSqlStmtdr
          From :StmtString;
       endsr;

       begsr clsdrcursor;
       exec sql
        close drcursor;
       endsr;

       begsr opndrcursor;
       exec sql
        open drcursor
          using :#drcomp,
                :#drlocn,
                :#drsupl,
                :#drsuplsub,
                :#drprod;
       endsr;

       Begsr IntSQLStmtdr;
       String = *blanks;
       String =   'Select * +
                   From K_Dealper +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr IntSQLStmtp2;
       String = *blanks;
       String =   'Select pr_altsrce, pr_tempory, pr_buyr, pr_supl +
                   pr_suplsub +
                   From K_product +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrchp2;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pr_comp = ? and +
                     pr_locn = ? and +
                     pr_prod = ? +
                     Order by pr_comp, +
                              pr_locn, +
                              pr_prod';
       endsr;

       begsr dclp2cursor;
       exec sql
        declare p2cursor Cursor
         for DynSQLStmtp2;
       endsr;

       //begsr opnp2cursor2;
       //exec sql
       // open p2cursor
       //   using :lda_comp,
       //         :polocnfrm,
       //         :pr_supl;
       //         :pr_suplsub;
       //        :pr_prod;
       //endsr;

       begsr opnp2cursor;
       exec sql
        open p2cursor
          using :lda_comp,
                :pq_locn,
                :pq_prod;
       endsr;

       begsr opnp2cursor2;
       exec sql
        open p2cursor
          using :lda_comp,
                :polocnfrm,
                :pq_prod;
       endsr;

       begsr PrepDynSQLStmtp2;
       exec sql
        Prepare DynSqlStmtp2
          From :StmtString;
       endsr;

       begsr clsp2cursor;
       exec sql
        close p2cursor;
       endsr;
      /end-free
