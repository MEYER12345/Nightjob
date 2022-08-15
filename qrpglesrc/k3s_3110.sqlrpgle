      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   Copyright (C) 1996-2016 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_3110
      **   Type: ILE RPG Program
      **   Desc: Selected products review - build batches
      **
      *****************************************************************
      **
      **   The purpose of this program is to build batches for the
      **   selected products review process. Both the header and detail
      **   files will be built.
      **
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d pr_test         s              8  0
     d xc_test         s              8  0
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
     d zz_diff         s              9  2
     d zz_histavg      s              9  2
     d zz_dmnddif      s             13  2
     d OC1_done        s              1    inz(*blanks)
     d zz_pctavg       s             13  2
     d location        s                   like(pr_locn)
     d per_hist        s              7  0 dim(52)
     d per_fact        s              4  2 dim(52)
     d point           s              3  0
     d zz_factr00      s              4  2
     d zz_dmd1         s              7  0
     d zz_factr01      s              4  2
     d zz_dmd2         s              7  0
     d zz_factr02      s              4  2
     d zz_dmd3         s              7  0
     d zz_factr03      s              4  2
     d #once           s              1                                         once routine
     d resultback      s              1a                                        once routine
     d nogo            s              1                                         once routine
     d prod_count      s                   like(pb_prodsel)                     count products
     d take_prod       s              1                                         take product flag
     d first_time      s              1                                         first in batch
     d one_delete      s              1                                         first in batch
     d date_ok         s              1                                         take product flag
     d dmd_yr1         s              1                                         result of year 1
     d dmd_yr2         s              1                                         result of year 2
     d dmd_yr3         s              1                                         result of year 3
     d year_one        s                   like(pr_birth)                       one year back
     d year_two        s                   like(pr_birth)                       two years back
     d year_three      s                   like(pr_birth)                       three years back
     d year_ok         s              1                                         year test ok
     d adjust_no       s                   like(pr_formanl)                     manual adjustment
     d adjust_ok       s              1                                         adjust test ok
     d x_test          s              8
     d p_test          s              8
     d lstordr         s             10
     d number          s              1                                         once routine
     d r1dlyouts       s                   like(r1_dlyouts)
     d r1qtyohnd       s                   like(r1_qtyohnd)
     d r1qtyoord       s                   like(r1_qtyoord)
     d gather_1        s                   like(pr_locn)                        gather to 1 location
     d occur_XX        s              2a                                        occurance XX value
     d occur_YY        s              2a                                        occurance YY value
     d occur_TP        s              1a                                        occurance type > , =
     d pgmname         s             10
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz

     d save_buyr       s                   like(pb_reqbuyr)                     saved buy group
     d save_regn       s                   like(pb_reqregn)                     saved region
     d save_locn       s                   like(pb_reqlocn)                     saved location
     d save_supl       s                   like(pb_reqsupl)                     saved supplier
     d save_sub        s                   like(pb_reqsub)                      saved supplier
     d save_user       s                   like(pb_actuser)                     saved supplier
     d save_pe         s                   like(pr_endper)                      saved per end code

     d xc_perd12       s                   like(lc_perd12)                      saved per end code
     d xc_year12       s                   like(lc_year12)                      saved per end code
     d xc_perd13       s                   like(lc_perd13)                      saved per end code
     d xc_year13       s                   like(lc_year13)                      saved per end code
     d xc_perd52       s                   like(lc_perd52)                      saved per end code
     d xc_year52       s                   like(lc_year52)                      saved per end code

     d batch           s              7  0                                      new batch id
     d tmp_batch       s                   like(pb_batch)                       new batch id

     d buyr_brk        s                   like(pb_sepbuyr)                     break buy group
     d regn_brk        s                   like(pb_sepregn)                     break region
     d locn_brk        s                   like(pb_seplocn)                     break location
     d supl_brk        s                   like(pb_sepsupl)                     break supplier

     d OnHand1         s                   like(pr_qtyohnd)                     On Hand today
     d OnOrdr1         s                   like(pr_qtyoord)                     On Order today
     d DlyOuts1        s                   like(r1_dlyouts)                     On Order today
     d OnHand2         s                   like(pr_qtyohnd)                     On Hand yesterday
     d OnOrdr2         s                   like(pr_qtyoord)                     On Order yesterday
     d OnHand3         s                   like(pr_qtyohnd)                     On Hand before yest
     d OnOrdr3         s                   like(pr_qtyoord)                     On Order before yest

     d prodsez_nf      s               n
      * -------------------------------------------------- parameters passed
     d rq_mindolr      s                   like(pb_mindolr)                     min dollar
     d rq_minunit      s                   like(pb_minunit)                     min dollar
     d rq_szma12       s                   like(pb_szma12)                      min dollar
     d rq_szma13       s                   like(pb_szma13)                      min dollar
     d xx_sub          s                   like(pb_reqsub)                      request sub supl
     d xx_status       s                   like(pb_status)                      batch status
     d rq_hidevp       s                   like(pb_hidevp)                      min dollar
     d rq_szhilo       s                   like(pb_szhilo)                      min dollar
     d rq_szsens       s                   like(pb_szsens)                      min dollar
     d rq_suplgp1      s                   like(pr_group1)                      min dollar
     d rq_suplgp2      s                   like(pr_group1)                      min dollar
     d rq_suplgp3      s                   like(pr_group1)                      min dollar
     d rq_suplgp4      s                   like(pr_group1)                      min dollar
     d rq_suplgp5      s                   like(pr_group1)                      min dollar
     d rq_prodgp1      s                   like(pr_group1)                      min dollar
     d rq_prodgp2      s                   like(pr_group1)                      min dollar
     d rq_prodgp3      s                   like(pr_group1)                      min dollar
     d rq_prodgp4      s                   like(pr_group1)                      min dollar
     d rq_prodgp5      s                   like(pr_group1)                      min dollar
     d birth_test      s                   like(pr_birth)                       request program type
     d zz_histtot      s             13  2
     d cmsysdate       s               d                                        min dollar
     d tacodeds1       s            100a
     d taflag5         s              1  0
     d req3            s              3a

     d hold_actbuyr    s                   like(pb_actbuyr)
     d hold_actregn    s                   like(pb_actregn)
     d hold_actlocn    s                   like(pb_actlocn)
     d hold_actsupl    s                   like(pb_actsupl)
     d hold_actsub     s                   like(pb_actsub)

      * --------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------
     d product_rec   e ds                  ExtName(k_product)
     d prodsez_rec   e ds                  ExtName(k_prodsez)
     d locatns_rec   e ds                  ExtName(k_locatns)
     d suplier_rec   e ds                  ExtName(k_suplier)
     d prodsed_rec   e ds                  ExtName(k_prodsed)
     d prodseb_rec   e ds                  ExtName(k_prodseb)
     d dlyprod_rec   e ds                  ExtName(k_dlyprod)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3110        PI
     d  sbm_batch                     7
     d  comp                          1
     d  req_user                     10
     d  req_type                      3
     d  req_buyr                      5
     d  req_regn                      5
     d  req_locn                      5
     d  req_supl                     10
     d  req_sub                      10
     d  brk_buyr                      1
     d  brk_regn                      1
     d  brk_locn                      1
     d  brk_supl                      1
     d  req_descr                    30
     d  req_pgmtyp                    1
     d  xx_mindolr                    7
     d  xx_minunit                    7
     d  xx_suplgps                   52
     d  xx_prodgps                   52
     d  xx_hidevp                     3
     d  xx_szma12                     5
     d  xx_szma13                     5
     d  xx_szhilo                     3
     d  xx_szsens                     3
     d  xx_selview                    1
     d  forcint                       3  0

     d K3S_3QXX        PR                  extpgm(pgmname)
     d comp                           1
     d pr_locn                        5
     d pr_supl                       10
     d pr_suplsub                    10
     d pr_prod                       25
     d take_prod                      1

      /free
       //------------------------------------------------------ once routine
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       // Declare and open product cursor
       exsr dclprcursor;
       exsr clsprcursor;
       exsr opnprcursor;

       //only process products where:
       //       delete count has not started and
       //       no alternate source products allowed and
       //       company must be buyers company

       //--------------------------------------------------------- Main Loop
       //main loop
       Dow SQLSTT = SQLStateOk;

       //fetch next product record
         exec sql
          fetch next
           from prcursor
           into :product_rec;

         if SQLState = RowNotFound;
            leave;
         endif;

         if #once <> *on;
            #once =  *on;

       //break down 50 length parm into groups 1 - 5
            rq_suplgp1 = %subst(xx_suplgps:2:10);
            rq_suplgp2 = %subst(xx_suplgps:12:10);
            rq_suplgp3 = %subst(xx_suplgps:22:10);
            rq_suplgp4 = %subst(xx_suplgps:32:10);
            rq_suplgp5 = %subst(xx_suplgps:42:10);
            rq_prodgp1 = %subst(xx_prodgps:2:10);
            rq_prodgp2 = %subst(xx_prodgps:12:10);
            rq_prodgp3 = %subst(xx_prodgps:22:10);
            rq_prodgp4 = %subst(xx_prodgps:32:10);
            rq_prodgp5 = %subst(xx_prodgps:42:10);

       //first product in group
            if xx_hidevp = *blanks;
              xx_hidevp = *all'00';
            endif;

            if xx_szma12 = *blanks;
              xx_szma12 = *all'00';
            endif;

            if xx_szma13 = *blanks;
              xx_szma13 = *all'00';
            endif;

            if xx_szhilo = *blanks;
              xx_szhilo = *all'00';
            endif;

            if xx_szsens = *blanks;
              xx_szsens = *all'00';
            endif;

            if xx_mindolr = *blanks;
              xx_mindolr = *all'00';
            endif;

            if xx_minunit = *blanks;
              xx_minunit = *all'00';
            endif;

            rq_hidevp = %dec(xx_hidevp:3:1);
            rq_szma12 = %dec(xx_szma12:5:3);
            rq_szma13 = %dec(xx_szma13:5:3);
            rq_szhilo = %dec(xx_szhilo:3:2);
            rq_szsens = %dec(xx_szsens:3:2);
            rq_mindolr = %dec(xx_mindolr:7:0);
            rq_minunit = %dec(xx_minunit:7:0);

       //get company code
            exec sql
             select cm_sysdate
              into :cmsysdate
              from k_company
              where cm_comp = :comp
              fetch first row only;

       //establish date for testing Obsolete products
            birth_test = cmsysdate - %days(180);

       //move break alpha values into numeric values
            buyr_brk = %dec(brk_buyr:1:0);                                      //buyer break
            regn_brk = %dec(brk_regn:1:0);                                      //region break
            locn_brk = %dec(brk_locn:1:0);                                      //location break
            supl_brk = %dec(brk_supl:1:0);                                      //supplier break

       //call program to get next batch number to be used
            batch = 0;
            callp K3S_3120(comp:
                           batch);

       //prime company code
            pb_comp = comp;

            save_buyr = pr_buyr;
            save_regn = pr_regn;
            save_locn = pr_locn;
            save_supl = pr_supl;
            save_sub = pr_suplsub;
            save_pe   = pr_endper;

       //   get user ID based upon buy group
            exec sql
             select ta_codeds1
               into :tacodeds1
               from k_tablcod
               where ta_comp = :comp and
                     ta_codetyp = 'BUY' and
                     ta_codeval = :save_buyr
               fetch first row only;
            if SQLState = SQLStateOk;
               save_user = tacodeds1;
            endif;
            SQLState = SQLStateOk;
         endif;
       // ------------------------------------------------- selection criteria

       //  only process products where:
       //  delete count has not started and
       //  no alternate source products allowed and
       //  company must be buyers company

         if (pr_deltcnt = 0)  and
            (pr_altsrce = 0)  and
            (pr_comp = comp);

               take_prod = *off;
               nogo = *off;
               if (rq_suplgp1 <> *blanks) or
                  (rq_suplgp2 <> *blanks) or
                  (rq_suplgp3 <> *blanks) or
                  (rq_suplgp4 <> *blanks) or
                  (rq_suplgp5 <> *blanks) or
                  (req_type = 'LTM');
                      if (sp_locn <> pr_locn) or
                         (sp_supl <> pr_supl) or
                         (sp_suplsub <> pr_suplsub);
                         exec sql
                          select *
                          into :suplier_rec
                          from k_suplier
                          where sp_comp = :pr_comp and
                                sp_locn = :pr_locn and
                                sp_supl = :pr_supl and
                                sp_suplsub = :pr_suplsub
                          fetch first row only;
                      endif;
               endif;

               if (rq_mindolr <> 0) and
                  (pr_ansale$ < rq_mindolr);
                     nogo = *on;
               endif;

               if (rq_minunit <> 0) and
                  (pr_ansaleu < rq_minunit);
                     nogo = *on;
               endif;

               if (rq_prodgp1 <> *blanks) and
                  (pr_group1 <> rq_prodgp1);
                     nogo = *on;
               endif;

               if (rq_prodgp2 <> *blanks) and
                  (pr_group2 <> rq_prodgp2);
                     nogo = *on;
               endif;

               if (rq_prodgp3 <> *blanks) and
                  (pr_group3 <> rq_prodgp3);
                     nogo = *on;
               endif;

               if (rq_prodgp4 <> *blanks) and
                  (pr_group4 <> rq_prodgp4);
                     nogo = *on;
               endif;

               if (rq_prodgp5 <> *blanks) and
                  (pr_group5 <> rq_prodgp5);
                     nogo = *on;
               endif;

               if (rq_suplgp1 <> *blanks) and
                  (rq_suplgp1 <> sp_group1);
                     nogo = *on;
               endif;

               if (rq_suplgp2 <> *blanks) and
                  (rq_suplgp2 <> sp_group2);
                     nogo = *on;
               endif;

               if (rq_suplgp3 <> *blanks) and
                  (rq_suplgp3 <> sp_group3);
                     nogo = *on;
               endif;

               if (rq_suplgp4 <> *blanks) and
                  (rq_suplgp4 <> sp_group4);
                     nogo = *on;
               endif;

               if (rq_suplgp5 <> *blanks) and
                  (rq_suplgp5 <> sp_group5);
                   nogo = *on;
               endif;

               if (rq_hidevp <> 0) and
                  (pr_fordevp < rq_hidevp) and
                  (req_type = 'HDV');
                      nogo = *on;
               endif;

               if (rq_hidevp <> 0) and
                  (pr_fordevp > rq_hidevp) and
                  (req_type = 'LDV');
                      nogo = *on;
               endif;

               if (req_type = 'ZCG') and
                  (pr_forcint = 52) or
                  (req_type = 'ZNW') and
                  (pr_forcint = 52) or
                  (req_type = 'ZRM') and
                  (pr_forcint = 52) or
                  (req_type = 'Z00') and
                  (pr_forcint = 52) or
                  (req_type = 'Z10') and
                  (pr_forcint = 52);
                     nogo = *on;
               endif;

               if req_type = 'QRY';
                  if req_buyr <> *blanks and
                     pr_buyr <> req_buyr;
                       nogo = *on;
                  endif;

                  if req_regn <> *blanks and
                     pr_regn <> req_regn;
                       nogo = *on;
                  endif;

                  if req_locn <> *blanks and
                     pr_locn <> req_locn;
                       nogo = *on;
                  endif;

                  if req_supl <> *blanks and
                     pr_supl <> req_supl;
                       nogo = *on;
                  endif;
                  if req_sub <> *blanks and
                     req_sub <> '*' and
                     pr_suplsub <> req_sub;
                       nogo = *on;
                  endif;
               endif;


               if nogo = *off;

                  select;

       //------------------------------------------------- seasonality check

                    when req_type = 'ZCG' or
                         req_type = 'ZNW' or
                         req_type = 'ZRM';
                           resultback = *blanks;
                           callp K3S_3581(comp:
                                          pr_locn:
                                          pr_suplorg:
                                          pr_suplors:
                                          pr_prod:
                                          resultback:
                                          xx_szma12:
                                          xx_szma13:
                                          xx_szhilo:
                                          xx_szsens:
                                          xx_mindolr:
                                          xx_minunit);
                           if req_type = 'ZCG' and
                              resultback = '2' or
                              req_type = 'ZNW' and
                              resultback = '1' or
                              req_type = 'ZRM' and
                              resultback = '3';
                                take_prod = *on;
                           endif;

       //------------------------------------------------ Order cycle checks

                    when req_type = 'OC1' and
                        OC1_done = *blanks;                                     //company id
                           callp K3S_2046(comp:
                                         batch:
                                         req_user:
                                         req_type:
                                         req_buyr:
                                         req_regn:
                                         req_locn:
                                         req_supl:
                                         req_sub:
                                         brk_buyr:
                                         brk_regn:
                                         brk_locn:
                                         brk_supl:
                                         req_descr:
                                         req_pgmtyp:
                                         xx_suplgps:
                                         xx_selview);


                           OC1_done = 'Y';                                      //new batch id


       //---------------------------------------- profile factors with 0.00

                    when req_type = 'Z00';
                         if pr_seasonl <> *blanks;
                           exec sql
                            select *
                            into :prodsez_rec
                            from k_prodsez
                            where pz_comp = :pr_comp and
                                  pz_seasonl = :pr_seasonl
                            fetch first row only;

                            if SQLState = SQLStateOk;
                              if pr_forcint = 12;
                                if pz_factr01 = 0 OR
                                   pz_factr02 = 0 OR
                                   pz_factr03 = 0 OR
                                   pz_factr04 = 0 OR
                                   pz_factr05 = 0 OR
                                   pz_factr06 = 0 OR
                                   pz_factr07 = 0 OR
                                   pz_factr08 = 0 OR
                                   pz_factr09 = 0 OR
                                   pz_factr10 = 0 OR
                                   pz_factr11 = 0 OR
                                   pz_factr12 = 0;
                                     take_prod = *on;
                                endif;
                              endif;

                              if pr_forcint = 13;
                                if pz_factr01 = 0 OR
                                   pz_factr02 = 0 OR
                                   pz_factr03 = 0 OR
                                   pz_factr04 = 0 OR
                                   pz_factr05 = 0 OR
                                   pz_factr06 = 0 OR
                                   pz_factr07 = 0 OR
                                   pz_factr08 = 0 OR
                                   pz_factr09 = 0 OR
                                   pz_factr10 = 0 OR
                                   pz_factr11 = 0 OR
                                   pz_factr12 = 0 OR
                                   pz_factr13 = 0;
                                     take_prod = *on;
                                endif;
                              endif;

                            endif;
                         endif;


       //---------------------------------------- profile small factors

                    when req_type = 'Z10';
                          if pr_seasonl <> *blanks;

                             exec sql
                              select *
                              into :prodsez_rec
                              from k_prodsez
                              where pz_comp = :pr_comp and
                                    pz_seasonl = :pr_seasonl
                              fetch first row only;

                              if SQLState = SQLStateOk;
                                 if pr_forcint = 12;
                                   if (pz_factr01 > 0 AND
                                       pz_factr01 <= rq_szhilo) OR
                                      (pz_factr02 > 0 AND
                                       pz_factr02 <= rq_szhilo) OR
                                      (pz_factr03 > 0 AND
                                       pz_factr03 <= rq_szhilo) OR
                                      (pz_factr04 > 0 AND
                                       pz_factr04 <= rq_szhilo) OR
                                      (pz_factr05 > 0 AND
                                       pz_factr05 <= rq_szhilo) OR
                                      (pz_factr06 > 0 AND
                                       pz_factr06 <= rq_szhilo) OR
                                      (pz_factr07 > 0 AND
                                       pz_factr07 <= rq_szhilo) OR
                                      (pz_factr08 > 0 AND
                                       pz_factr08 <= rq_szhilo) OR
                                      (pz_factr09 > 0 AND
                                       pz_factr09 <= rq_szhilo) OR
                                      (pz_factr10 > 0 AND
                                       pz_factr10 <= rq_szhilo) OR
                                      (pz_factr11 > 0 AND
                                       pz_factr11 <= rq_szhilo) OR
                                      (pz_factr12 > 0 AND
                                       pz_factr12 <= rq_szhilo);
                                         take_prod = *on;
       // don't want products in both Z00 and Z10, so check it
                                         if pz_factr01 = 0 OR
                                            pz_factr02 = 0 OR
                                            pz_factr03 = 0 OR
                                            pz_factr04 = 0 OR
                                            pz_factr05 = 0 OR
                                            pz_factr06 = 0 OR
                                            pz_factr07 = 0 OR
                                            pz_factr08 = 0 OR
                                            pz_factr09 = 0 OR
                                            pz_factr10 = 0 OR
                                            pz_factr11 = 0 OR
                                            pz_factr12 = 0;
                                              take_prod = *off;
                                         endif;
                                   endif;
                                 endif;

                                 if pr_forcint = 13;
                                   if (pz_factr01 > 0 AND
                                      pz_factr01 <= rq_szhilo) OR
                                      (pz_factr02 > 0 AND
                                      pz_factr02 <= rq_szhilo) OR
                                      (pz_factr03 > 0 AND
                                      pz_factr03 <= rq_szhilo) OR
                                      (pz_factr04 > 0 AND
                                      pz_factr04 <= rq_szhilo) OR
                                      (pz_factr05 > 0 AND
                                      pz_factr05 <= rq_szhilo) OR
                                      (pz_factr06 > 0 AND
                                      pz_factr06 <= rq_szhilo) OR
                                      (pz_factr07 > 0 AND
                                      pz_factr07 <= rq_szhilo) OR
                                      (pz_factr08 > 0 AND
                                      pz_factr08 <= rq_szhilo) OR
                                      (pz_factr09 > 0 AND
                                      pz_factr09 <= rq_szhilo) OR
                                      (pz_factr10 > 0 AND
                                      pz_factr10 <= rq_szhilo) OR
                                      (pz_factr11 > 0 AND
                                      pz_factr11 <= rq_szhilo) OR
                                      (pz_factr12 > 0 AND
                                      pz_factr12 <= rq_szhilo) OR
                                      (pz_factr13 >  0 AND
                                      pz_factr13 <= rq_szhilo);
                                         take_prod = *on;
       // don't want products both Z00 and Z10, so check it
                                         if pz_factr01 = 0 OR
                                            pz_factr02 = 0 OR
                                            pz_factr03 = 0 OR
                                            pz_factr04 = 0 OR
                                            pz_factr05 = 0 OR
                                            pz_factr06 = 0 OR
                                            pz_factr07 = 0 OR
                                            pz_factr08 = 0 OR
                                            pz_factr09 = 0 OR
                                            pz_factr10 = 0 OR
                                            pz_factr11 = 0 OR
                                            pz_factr12 = 0 OR
                                            pz_factr13 = 0;
                                              take_prod = *off;
                                         endif;
                                   endif;
                                 endif;
                              endif;
                          endif;

       //------------------------------------------------- forecast too high

                    when req_type = 'FHI';
                          exsr $_date_chk;
                          if date_ok = *on;
                            exsr $_hist_chk;
                            if pr_forcast * zz_factr00 > 2 *
                               ((zz_dmd1 * zz_factr01 +
                               zz_dmd2 * zz_factr02 +
                               zz_dmd3 * zz_factr03) / 3);

                                 take_prod = *on;
                            endif;
                          else;
                             take_prod = *off;
                          endif;


       //------------------------------------------------- forecast too low

                    when req_type = 'FLO';
                            exsr $_date_chk;
                            if date_ok = *on;
                              exsr $_hist_chk;
                              if pr_forcast * zz_factr00 < .5 *
                                 ((zz_dmd1 * zz_factr01 +
                                 zz_dmd2 * zz_factr02 +
                                 zz_dmd3 * zz_factr03) / 3);

                                   take_prod = *on;
                              endif;
                            else;
                               take_prod = *off;
                            endif;


       //------------------------------------------------- obsolete products

                    when req_type = 'OBS';
                          if pr_accdem = *zeros;
                            zz_histype = *zeros;
                            callp K3S_3700(comp:
                                           pr_locn:
                                           pr_suplorg:
                                           pr_suplors:
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
                            zz_histtot = zz_hist01 + zz_hist02 +
                            zz_hist03 + zz_hist04 + zz_hist05 +
                            zz_hist06;
                            if zz_histtot = *zeros AND
                               pr_birth < birth_test;

                                 take_prod = *on;
                            endif;
                          endif;


       //------------------------------------------------- product overstock

                    when req_type = 'OVR';

       //       overstock exists
                          if pr_overunt > 0;
                            take_prod = *on;
                          endif;
       // -------------------------------- product system status - Discontinued
                    when req_type = 'PSD';

                          if pr_sysstat = 'D';
                             take_prod = *on;
                          endif;
       // -------------------------------- product system status - Lumpy
                    when req_type = 'PSL';

                          if pr_sysstat = 'L';
                             take_prod = *on;
                          endif;
       // -------------------------------- product system status - New
                    when req_type = 'PSN';

                          if pr_sysstat = 'N';
                             take_prod = *on;
                          endif;
      // -------------------------------- product system status - Regular
                    when req_type = 'PSR';

                          if pr_sysstat = 'R';
                             take_prod = *on;
                          endif;
       // -------------------------------- product system status - Slow
                    when req_type = 'PSS';

                          if pr_sysstat = 'S';
                             take_prod = *on;
                          endif;
       // -------------------------------- product user status - Frozen
                    when req_type = 'PUF';

                          if pr_usrstat = 'F';
                             take_prod = *on ;
                          endif;
       // -------------------------------- product user status - Manual
                    when req_type = 'PUM';

                          if pr_usrstat = 'M';
                             take_prod = *on;
                          endif;
       // -------------------------------- product user status - Probation

                    when req_type = 'PUP' ;

                          if pr_usrstat = 'P';
                            take_prod = *on;
                          endif ;

       // -------------------------------- product user status - Watch

                   when req_type = 'PUW';

                         if pr_usrstat = 'W';
                            take_prod = *on;
                         endif;

       // ------------------------------------ Gather OV checks to 1 location

                   when req_type = 'OVG';

                         gather_1 = %subst(req_descr:1:5);
                         take_prod = *off;
                         callp K3S_OVG1(comp:
                                        pr_locn:
                                        pr_supl:
                                        pr_suplsub:
                                        pr_prod:
                                        gather_1:
                                        take_prod);

       // ------------------------------------ Demand occurances-All products
       //                                                      or
       //                                                 -Discontinued

                   when req_type = 'DOA' or
                        req_type = 'DOD';

                         occur_XX = %subst(req_descr:1:2);
                         occur_YY = %subst(req_descr:13:2);
                         occur_TP = %subst(req_descr:3:1);

                         take_prod = *off;
                         callp K3S_DMOC(comp:
                                        pr_locn:
                                        pr_supl:
                                        pr_suplsub:
                                        pr_prod:
                                        req_type:
                                        occur_XX:
                                        occur_YY:
                                        occur_TP:
                                        take_prod);

       // ------------------------------------ Products link type D

                   when req_type = 'PDF' or
                        req_type = 'PDT';

                         take_prod = *off;
                         callp K3S_PDTYP(comp:
                                         pr_locn:
                                         pr_supl:
                                         pr_suplsub:
                                         pr_prod:
                                         req_type:
                                         take_prod);

       //------------------------------------------------- out of stock

                    when req_type = 'OUT';

       //       out of stock last night
                          lstordr = %char(pr_lstordr:*ISO);
                          if pr_qtyohnd = 0    AND
                             pr_sysstat <> 'D' AND
                             pr_sysstat <> 'N' AND
                             pr_usrstat <> 'M' AND
                             lstordr    <> '0001-01-01';
                               take_prod = *on;
                          endif;

       //----------------------------------------------- Lead time exceptions

                    when req_type = 'LTM';

       //       Lead time exceptions
                          if pr_leadtm > 1.5 * sp_leadtmo and
                             pr_sysstat <> 'D';
                               take_prod = *on;
                          endif;


       //----------------------------------------------- seasonality testing

                    when req_type = 'SEA';

       //       seasonl profile exists
                          if pr_seasonl <> *blanks;
                            take_prod = *on;
                          endif;

       //----------------------------------------------- Lost sales testing

                    when req_type = 'LS1' or
                           req_type = 'LS2' or
                           req_type = 'LS3';

                           exec sql
                              select r1_dlyouts, r1_qtyohnd
                                into :r1dlyouts, :r1qtyohnd
                                from k_dlyprod
                                where r1_comp = :pr_comp and
                                      r1_locn = :pr_locn and
                                      r1_suplusr = :pr_suplusr and
                                      r1_suplusb = :pr_suplusb and
                                      r1_prod = :pr_prod and
                                      r1_birth = :cmsysdate
                                fetch first row only;
                           if SQLState = SQLStateOK and r1dlyouts > 0;
       //       get last nights daily interface record
                             if (req_type = 'LS1' and r1qtyohnd > 0) or
                                (req_type = 'LS2' and r1qtyohnd = 0) or
                                (req_type = 'LS3');
                                  take_prod = *on;
                             endif;
                           endif;

       //----------------------------------------------- No demand testing

                    when req_type = 'ND1' or
                              req_type = 'ND2' or
                              req_type = 'ND3';

       //       no weekly products
                          if pr_forcint = 12 or
                             pr_forcint = 13;

       //       skip Manual and Discontinued
                             if pr_usrstat <> 'M' and
                                pr_sysstat <> 'D';

       //       must have an average
                               if pr_forcast > 0;

       //       exclude products with current period demand
                                 if pr_accdem = 0;

       //       exclude no cost products
                                   if pr_costreg > 0;

       //       must have old birth date
                                     year_one = pr_lastupd - %years(1);
                                     year_two = pr_lastupd - %years(2);
                                     year_three = pr_lastupd - %years(3);
                                     year_ok = *off;
                                     select;
                                       when req_type = 'ND1';
                                         if pr_birth < year_one;
                                           year_ok = *on;
                                         endif;
                                       when req_type = 'ND2';
                                         if pr_birth < year_two;
                                           year_ok = *on;
                                         endif;
                                       when req_type = 'ND3';
                                         if pr_birth < year_three;
                                           year_ok = *on;
                                         endif;
                                     endsl;
                                     if year_ok = *on;

       //       no recent adjustment to average
                                       adjust_ok = *off;
                                       select;
                                         when req_type = 'ND1';
                                           if pr_formanl = adjust_no or
                                              pr_formanl < year_one;
                                                adjust_ok = *on;
                                           endif;
                                         when req_type = 'ND2';
                                           if pr_formanl = adjust_no or
                                              pr_formanl < year_two;
                                                adjust_ok = *on;
                                           endif;
                                         when req_type = 'ND3';
                                           if pr_formanl = adjust_no or
                                              pr_formanl < year_three;
                                                adjust_ok = *on;
                                           endif;
                                       endsl;
                                       if adjust_ok = *on;

                                         dmd_yr1 = '0';
                                         dmd_yr2 = '0';
                                         dmd_yr3 = '0';
                                         callp K3S_3582(comp:
                                                        pr_locn:
                                                        pr_suplorg:
                                                        pr_suplors:
                                                        pr_prod:
                                                        dmd_yr1:
                                                        dmd_yr2:
                                                        dmd_yr3);


                                         if (req_type = 'ND1' and
                                            dmd_yr1 = '0') or
                                            (req_type ='ND2' and
                                            dmd_yr1 = '0' and
                                            dmd_yr2 = '0') or
                                            (req_type = 'ND3' and
                                            dmd_yr1 = '0' and
                                            dmd_yr2 = '0' and
                                            dmd_yr3 = '0');
                                              take_prod = *on;
                                         endif;
                                       endif;
                                     endif;
                                   endif;
                                 endif;
                               endif;
                             endif;
                           endif;

       //---------------------------- Customer controlled exit point batches

       //  req_type of 'Q01 through Q99' are possible (not QRY batches)
                    when %subst(req_type:1:1) = 'Q' AND
                                %subst(req_type:2:2) <> 'RY';

                          take_prod = *off;
                          pgmname = 'K3S_3' + req_type;
                          callp K3S_3QXX(comp:
                                         pr_locn:
                                         pr_supl:
                                         pr_suplsub:
                                         pr_prod:
                                         take_prod);

       //----------------------------------------------- Inventory Float

                    when req_type = 'IVF';
       //
                       exsr InzInpSrch;
       //initialize StmtString
                       exsr intSQLStmt;
       //prepare statement
                       exsr prepDynSQLStmt;
       //if good prep
                       if SQLState = SQLStateOK;
                          exsr opnr1cursor;
       //if good open
                          if SQLState = SQLStateOk;

       // get last nights daily interface record
                             exec sql
                               fetch next
                                  from r1cursor
                                  into :dlyprod_rec;
                             if SQLState = SQLStateOk and
                              r1_birth = cmsysdate;
                                   OnHand1 = r1_qtyohnd;
                                   OnOrdr1 = r1_qtyoord;
                                   DlyOuts1= r1_dlyouts;

       // get yesterdays daily interface record
                                   exec sql
                                   fetch next
                                      from r1cursor
                                      into :dlyprod_rec;

                                   if SQLState = RowNotFound;
                                      OnHand2 = r1_qtyohnd;
                                      OnOrdr2 = r1_qtyoord;

       // get the day before yesterdays daily int record
                                      exec sql
                                        fetch next
                                        from r1cursor
                                        into :dlyprod_rec;

                                        if SQLState = SQLStateOK;
                                           OnHand3 = r1_qtyohnd;
                                           OnOrdr3 = r1_qtyoord;

                                           if (OnHand1 = OnHand3)     AND
                                              (OnHand1 + OnHand3) > 0 AND
                                              (OnHand2 = 0)           AND
                                              DlyOuts1 > 0;

                                              take_prod = *on;
                                           endif;
                                        endif;
                                   endif;
                             endif;
                             exsr clsr1cursor;
                          endif;
                       endif;
       //----------------------------------------------- high devation %

                    when req_type = 'HDV';

                          take_prod = *on;


       //----------------------------------------------- low  devation %

                    when req_type = 'LDV';

                          take_prod = *on;



       //----------------------------------------------- QRY testing

                    when req_type = 'QRY';

                          take_prod = *on;



       //------------------------------------- period end history exceptions

                    when req_type = 'PE ';

                          take_prod = *on;

                  endsl;

       //----------------------------------------------- build detail record

       // product has met criteria above, so put it into a batch
                  if take_prod = *on;


       //    has a break occured?
                     if ((save_buyr <> pr_buyr) and
                         (buyr_brk = 1))                   or

                        ((save_regn <> pr_regn) and
                         (regn_brk = 1))                   or

                        ((save_locn <> pr_locn) and
                         (locn_brk = 1))                   or

                        ((save_supl <> pr_supl) and
                         (supl_brk = 1))                   or

                        ((save_sub <> pr_suplsub) and
                         (supl_brk = 1) and
                         (req_sub <> *blanks)) or

                        ((save_pe   <> pr_endper) and
                         (req_type = 'PE '));

                           exsr $_header;

                     endif;

                     prod_count += 1;

                     req3 = req_type;
                     if req_type = 'PE ';
                        req3 = 'PE' + save_pe;
                     endif;

                     exec sql
                       insert into k_prodsed
                         (pd_comp,
                          pd_reqtype,
                          pd_batch,
                          pd_birth,
                          pd_lastupd,
                          pd_actbuyr,
                          pd_actregn,
                          pd_actlocn,
                          pd_actsupl,
                          pd_actsub,
                          pd_actuser,
                          pd_prod,
                          pd_prodseq,
                          pd_ansale$,
                          pd_ansaleu,
                          pd_desc1,
                          pd_review)
                       values(:comp,
                              :req3,
                              :batch,
                              :cmsysdate,
                              :cmsysdate,
                              :pr_buyr,
                              :pr_regn,
                              :pr_locn,
                              :pr_supl,
                              :pr_suplsub,
                              :save_user,
                              :pr_prod,
                              :pr_prodseq,
                              :pr_ansale$,
                              :pr_ansaleu,
                              :pr_desc1,
                              0);
                  endif;

       //selection criteria
               endif;
         endif;

       enddo;
       //-----------------------------------------------
       //last record check
       //if *inlr = *on;
          if one_delete = ' ';
            xx_status = 'P';
            tmp_batch = %dec(sbm_batch:7:0);

            exec sql
             Delete
               From k_prodseb
               Where pb_comp = :comp and
                     pb_status = :xx_status and
                     pb_actbuyr = :req_buyr and
                     pb_batch = :tmp_batch and
                     pb_actregn = :req_regn and
                     pb_actlocn = :req_locn and
                     pb_actsupl = :req_supl and
                     pb_actsub = :req_sub;
               if SQLState = SQLStateOK;
                  one_delete = '1';
               endif;
          endif;
          if prod_count > *zeros;
             exsr $_header;
          endif;
          pr_prod = *blanks;
          resultback = *blanks;
          callp K3S_3581(comp:
                         pr_locn:
                         pr_suplorg:
                         pr_suplors:
                         pr_prod:
                         resultback:
                         xx_szma12:
                         xx_szma13:
                         xx_szhilo:
                         xx_szsens:
                         xx_mindolr:
                         xx_minunit);
       //endif;
       exsr clsprcursor;
       *inlr = *on;
       ///////////////////////////////////////////// create new batch header
       begsr $_header;
       if prod_count > *zeros;

       // if buyr_brk = 0;
       //    hold_actbuyr = *blanks;
       //    hold_actuser = req_user;
       // else;
       //    hold_actbuyr = save_buyr;
       //    hold_actuser = save_user;
       // endif;

       // if regn_brk = 0;
       //    hold_actregn = *blanks;
       // else;
       //    hold_actregn = save_regn;
       // endif;

       // if locn_brk = 0;
       //    hold_actlocn = *blanks;
       // else;
       //    hold_actlocn = save_locn;
       // endif;

       // if supl_brk = 0;
       //    hold_actsupl = *blanks;
       //    hold_actsub  = *blanks;
       // else;
       //    hold_actsupl = save_supl;
       //    hold_actsub = save_sub;
       // endif;

       // if req_buyr <> *blanks;
       //    hold_actbuyr = req_buyr;
       // endif;

       // if req_regn <> *blanks;
       //    hold_actregn = req_regn;
       // endif;

       // if req_locn <> *blanks;
       //    hold_actlocn = req_locn;
       // endif;

       // if req_supl <> *blanks;
       //    hold_actsupl = req_supl;
       // endif;

       // if req_sub <> *blanks;
       //    hold_actsub = req_sub;
       //    if req_sub = '*' and
       //       supl_brk = 1;
       //          hold_actsub = save_sub;
       //    endif;
       // endif;

       // exec sql
       //   select *
       //     into prodseb_rec
       //     from k_prodseb
       //     where pb_comp = :comp and
       //           pb_status = 'O' and
       //           pb_actbuyr = :hold_actbuyr and
       //           pb_batch = :batch and
       //           pb_actregn = :hold_actregn and
       //           pb_actlocn = :hold_actlocn and
       //           pb_actsupl = :hold_actsupl and
       //           pb_actsub = :hold_actsub
       //     fetch first row only;

             pb_reqtype = req_type;

             if req_type = 'PE ';
               pb_reqtype = 'PE' + save_pe;
             endif;

             pb_reqbuyr = req_buyr;
             pb_reqregn = req_regn;
             pb_reqlocn = req_locn;
             pb_reqsupl = req_supl;

             if  req_sub <> *blanks;
                 pb_reqsub = req_sub;
                 if req_sub = '*' and
                    supl_brk = 1;
                    pb_reqsub = save_sub;
                 endif;
             endif;
             pb_requser = req_user;
             pb_batch   = batch;
             pb_birth   = cmsysdate;
             pb_lastupd = cmsysdate;

             if buyr_brk = 0;
                pb_actbuyr = *blanks;
                pb_actuser = req_user;
             else;
                pb_actbuyr = save_buyr;
                pb_actuser = save_user;
             endif;

             if regn_brk = 0;
                pb_actregn = *blanks;
             else;
                pb_actregn = save_regn;
             endif;

             if locn_brk = 0;
                pb_actlocn = *blanks;
             else;
                pb_actlocn = save_locn;
             endif;

             if supl_brk = 0;
                pb_actsupl = *blanks;
                pb_actsub  = *blanks;
             else;
                pb_actsupl = save_supl;
                pb_actsupl = save_sub;
             endif;

             if req_buyr <> *blanks;
                pb_actbuyr = req_buyr;
       //       hold_actbuyr = req_buyr;
             endif;

             if req_regn <> *blanks;
                pb_actregn = req_regn;
             endif;

             if req_locn <> *blanks;
                pb_actlocn = req_locn;
             endif;

             if req_supl <> *blanks;
                pb_actsupl = req_supl;
             endif;

             if req_sub <> *blanks;
                pb_actsub = req_sub;
                if req_sub = '*' and
                   supl_brk = 1;
                      pb_actsub = save_sub;
                endif;
             endif;

             hold_actbuyr = *blanks;
             hold_actregn = *blanks;
             hold_actlocn = *blanks;
             hold_actsupl = *blanks;
             hold_actsub = *blanks;

             hold_actbuyr = pb_actbuyr;
             hold_actregn = pb_actregn;
             hold_actlocn = pb_actlocn;
             hold_actsupl = pb_actsupl;
             hold_actsub = pb_actsub;

          exec sql
            select *
              into :prodseb_rec
              from k_prodseb
              where pb_comp = :comp and
                    pb_status = 'O' and
                    pb_actbuyr = :hold_actbuyr and
                    pb_batch = :batch and
                    pb_actregn = :hold_actregn and
                    pb_actlocn = :hold_actlocn and
                    pb_actsupl = :hold_actsupl and
                    pb_actsub = :hold_actsub
              fetch first row only;

          If SQLState = RowNotFound;

             pb_status  = 'O';
             pb_descr   = req_descr;

             if req_type = 'PE ';
               exec sql
                select ta_codeds1, ta_flag5
                  into :tacodeds1, :taflag5
                  from k_tablcod
                  where ta_comp = :comp and
                        ta_codetyp = 'SPR' and
                        ta_codeval = :pb_reqtype
                  fetch first row only;
               if SQLState = SQLStateOk;
                  pb_descr = tacodeds1;
          // auto close test
                  if taflag5 = 1;
                     if %subst(pb_reqtype:1:3) = 'PE6' or
                        %subst(pb_reqtype:1:3) = 'PE7' or
                        %subst(pb_reqtype:1:3) = 'PE8' or
                        %subst(pb_reqtype:1:3) = 'PE9';
                           pb_status  = 'C';
                     endif;
                  endif;
               endif;
               SQLState = SQLStateOk;
             endif;

             if %subst(req_type:1:2) = 'PE';
                pb_forcint = pr_forcint;
             else;
                pb_forcint = *zeros;
             endif;

             pb_prodsel = prod_count;
             pb_prodrem = prod_count;
             pb_produpd = 0;
             pb_pgmtype = req_pgmtyp;
             pb_bchbusy = 0;
             pb_sepbuyr = buyr_brk;
             pb_sepregn = regn_brk;
             pb_seplocn = locn_brk;
             pb_sepsupl = supl_brk;
             pb_mindolr = rq_mindolr;
             pb_minunit = rq_minunit;
             pb_szma12  = rq_szma12;
             pb_szma13  = rq_szma13;
             pb_szhilo  = rq_szhilo;
             pb_szsens  = rq_szsens;
             pb_hidevp  = rq_hidevp;
             pb_suplgp1 = rq_suplgp1;
             pb_suplgp2 = rq_suplgp2;
             pb_suplgp3 = rq_suplgp3;
             pb_suplgp4 = rq_suplgp4;
             pb_suplgp5 = rq_suplgp5;
             pb_prodgp1 = rq_prodgp1;
             pb_prodgp2 = rq_prodgp2;
             pb_prodgp3 = rq_prodgp3;
             pb_prodgp4 = rq_prodgp4;
             pb_prodgp5 = rq_prodgp5;
             pb_selview = %dec(xx_selview:1:0);

             pb_lckuser = *blanks;

             clear pb_lcktime;

             pb_usrfroz = *all' ';
             pb_usrmanl = *all' ';
             pb_usrprob = *all' ';
             pb_usrwatc = *all' ';
             pb_usrnone = *all' ';
             pb_sysdisc = *all' ';
             pb_syslump = *all' ';
             pb_sysnew  = *all' ';
             pb_sysregl = *all' ';
             pb_sysslow  = *all' ';
             pb_crtuser = *all' ';

             exsr insert_prodseb;
          endif;
       endif;

       //clear product count for next batch
       clear prod_count;

       batch = 0;

       //get batch number for next batch
       //call program to get next batch number to be used
       callp K3S_3120(comp:
                      batch);

       //first product in group;
       save_buyr = pr_buyr;
       save_regn = pr_regn;
       save_locn = pr_locn;
       save_supl = pr_supl;
       save_sub = pr_suplsub;
       save_pe   = pr_endper;

       //get user ID based upon buy group
       exec sql
        select ta_codeds1
          into :tacodeds1
          from k_tablcod
          where ta_comp = :comp and
                ta_codetyp = 'BUY' and
                ta_codeval = :save_buyr
          fetch first row only;
       if SQLState = SQLStateOk;
          save_user = tacodeds1;
       endif;

       endsr;

       //////////////////////////////////////////////// Edit type selection

       begsr $_hist_chk;

       zz_dmd1  = *zeros;
       zz_dmd2  = *zeros;
       zz_dmd3  = *zeros;
       zz_factr00 = *zeros;
       zz_factr01 = *zeros;
       zz_factr02 = *zeros;
       zz_factr03 = *zeros;
       per_fact = *zeros;
       per_hist = *zeros;
       prodsez_nf = *on;

       if pr_seasonl <> *blanks;

          exec sql
           select *
           into :prodsez_rec
           from k_prodsez
           where pz_comp = :pr_comp and
           pz_seasonl = :pr_seasonl
           fetch first row only;

          if SQLState = SQLStateOk;
             prodsez_nf = *off;
             per_fact(01) = pz_factr01;
             per_fact(02) = pz_factr02;
             per_fact(03) = pz_factr03;
             per_fact(04) = pz_factr04;
             per_fact(05) = pz_factr05;
             per_fact(06) = pz_factr06;
             per_fact(07) = pz_factr07;
             per_fact(08) = pz_factr08;
             per_fact(09) = pz_factr09;
             per_fact(10) = pz_factr10;
             per_fact(11) = pz_factr11;
             per_fact(12) = pz_factr12;
             per_fact(13) = pz_factr13;
             per_fact(14) = pz_factr14;
             per_fact(15) = pz_factr15;
             per_fact(16) = pz_factr16;
             per_fact(17) = pz_factr17;
             per_fact(18) = pz_factr18;
             per_fact(19) = pz_factr19;
             per_fact(20) = pz_factr20;
             per_fact(21) = pz_factr21;
             per_fact(22) = pz_factr22;
             per_fact(23) = pz_factr23;
             per_fact(24) = pz_factr24;
             per_fact(25) = pz_factr25;
             per_fact(26) = pz_factr26;
             per_fact(27) = pz_factr27;
             per_fact(28) = pz_factr28;
             per_fact(29) = pz_factr29;
             per_fact(30) = pz_factr30;
             per_fact(31) = pz_factr31;
             per_fact(32) = pz_factr32;
             per_fact(33) = pz_factr33;
             per_fact(34) = pz_factr34;
             per_fact(35) = pz_factr35;
             per_fact(36) = pz_factr36;
             per_fact(37) = pz_factr37;
             per_fact(38) = pz_factr38;
             per_fact(39) = pz_factr39;
             per_fact(40) = pz_factr40;
             per_fact(41) = pz_factr41;
             per_fact(42) = pz_factr42;
             per_fact(43) = pz_factr43;
             per_fact(44) = pz_factr44;
             per_fact(45) = pz_factr45;
             per_fact(46) = pz_factr46;
             per_fact(47) = pz_factr47;
             per_fact(48) = pz_factr48;
             per_fact(49) = pz_factr49;
             per_fact(50) = pz_factr50;
             per_fact(51) = pz_factr51;
             per_fact(52) = pz_factr52;
          endif;
       endif;

       if pr_suplsub = 'h';
          location = pr_cmblocn;
       else;
          location = pr_locn;
       endif;
       zz_histype = *zeros;
       callp K3S_3700(comp:
                      location:
                      pr_suplorg:
                      pr_suplors:
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
       per_hist(01) = zz_hist01;
       per_hist(02) = zz_hist02;
       per_hist(03) = zz_hist03;
       per_hist(04) = zz_hist04;
       per_hist(05) = zz_hist05;
       per_hist(06) = zz_hist06;
       per_hist(07) = zz_hist07;
       per_hist(08) = zz_hist08;
       per_hist(09) = zz_hist09;
       per_hist(10) = zz_hist10;
       per_hist(11) = zz_hist11;
       per_hist(12) = zz_hist12;
       per_hist(13) = zz_hist13;
       zz_factr00 = per_fact(pr_forcper);
       if zz_factr00 = 00.00;
          zz_factr00 = .10;
       endif;
       point = pr_forcper - 1;
       if point = 0;
          point = pr_forcint;
       endif;
       zz_dmd1 = per_hist(1);
       zz_factr01 = per_fact(point);
       if zz_factr01 = 00.00;
          zz_factr01 = .10;
       endif;
       point = point - 1;
       if point = 0;
          point = pr_forcint;
       endif;
       zz_dmd2 = per_hist(2);
       zz_factr02 = per_fact(point);
       if zz_factr02 = 00.00;
          zz_factr02 = .10;
       endif;
       point = point - 1;
       if point = 0;
          point = pr_forcint;
       endif;
       zz_dmd3 = per_hist(3);
       zz_factr03 = per_fact(point);
       if zz_factr03 = 00.00;
          zz_factr03 = .10;
       endif;

       if prodsez_nf;
          zz_factr00 = 1.00;
          zz_factr01 = 1.00;
          zz_factr02 = 1.00;
          zz_factr03 = 1.00;
       endif;

       endsr;

       //////////////////////////////////////////////// Edit type selection

       begsr $_date_chk;

       date_ok = *off;
       pr_test = *zeros;
       xc_test = *zeros;
       if pr_fstslyr > 0 and
          pr_fstslpr > 0;

          exec sql
           select *
           into :locatns_rec
           from k_locatns
           where lc_comp = :pr_comp and
                 lc_locn = :pr_locn
           fetch first row only;

           if SQLState = SQLStateOk;
              if pr_forcint = 12;
                 xc_perd12 = lc_perd12 - 3;
                 xc_year12 = lc_year12;
                 if xc_perd12 < 0;
                    xc_perd12 += 12;
                    xc_year12 = lc_year12 - 1;
                 endif;
                 p_test = %editc(pr_fstslyr:'X');
                 evalr p_test = %editc(pr_fstslpr:'X');
                 pr_test = %dec(p_test:8:0);

                 x_test = %editc(xc_year12:'X');
                 evalr x_test = %editc(xc_perd12:'X');
                 xc_test = %dec(x_test:8:0);
                 if pr_test <= xc_test;
                    date_ok = *on;
                 endif;
              endif;
              if pr_forcint = 13;
                 xc_perd13 = lc_perd13 - 3;
                 xc_year13 = lc_year13;
                 if xc_perd13 < 0;
                    xc_perd13 += 13;
                    xc_year13 = lc_year13 - 1;
                 endif;
                 p_test = %editc(pr_fstslyr:'X');
                 evalr p_test = %editc(pr_fstslpr:'X');
                 pr_test = %dec(p_test:8:0);

                 x_test = %editc(xc_year13:'X');
                 evalr x_test = %editc(xc_perd13:'X');
                 xc_test = %dec(x_test:8:0);
                 if pr_test <= xc_test;
                    date_ok = *on;
                 endif;
              endif;
              if pr_forcint = 52;
                 xc_perd52 = lc_perd52 - 3;
                 xc_year52 = lc_year52;
                 if xc_perd52 < 0;
                    xc_perd52 += 52;
                    xc_year52 = lc_year52 - 1;
                 endif;
                 p_test = %editc(pr_fstslyr:'X');
                 evalr p_test = %editc(pr_fstslpr:'X');
                 pr_test = %dec(p_test:8:0);

                 x_test = %editc(xc_year52:'X');
                 evalr x_test = %editc(xc_perd52:'X');
                 xc_test = %dec(x_test:8:0);
                 if pr_test <= xc_test;
                    date_ok = *on;
                 endif;
              endif;
            endif;
       endif;

       endsr;

       begsr dclprcursor;
       exec sql
       Declare prcursor cursor
         for
         select *
         from k_product
         Where pr_comp = :comp and
               pr_endper <> ' ' and
               pr_deltcnt = 0 and
               pr_altsrce = 0 and
               pr_forcint = :forcint
               Order by pr_buyr,
                        pr_endper,
                        pr_locn,
                        pr_supl,
                        pr_suplsub,
                        pr_prodseq;
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

       begsr insert_prodsed;

       exec sql

        insert into k_prodsed
        values(:prodsed_rec);

       endsr;

       begsr insert_prodseb;

       exec sql

        insert into k_prodseb
        values (:prodseb_rec);

       endsr;

       begsr dclr1cursor;

       exec sql
         declare r1cursor Cursor
            for DynSQLStmt;

       endsr;

       begsr opnr1cursor;
       exec sql
         open r1cursor
           using :pr_comp,
                 :pr_locn,
                 :pr_suplusr,
                 :pr_suplusb,
                 :pr_prod,
                 :cmsysdate;
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
         Prepare DynSqlStmt
            From :StmtString;
       endsr;

       Begsr InzInpSrch;

       InpSrchCnd = *blanks;
       InpSrchCnd = 'r1_comp = ? and +
                     r1_locn = ? and +
                     r1_suplusr = ? and +
                     r1_suplusb = ? and +
                     r1_prod = ? +
                     r1_birth <= ? +

                     Order by r1_comp, +
                              r1_locn, +
                              r1_suplusr, +
                              r1_suplusb, +
                              r1_prod, +
                              r1_birth desc';
       endsr;

       Begsr IntSQLStmt;

       String = *blanks;
       String =   'Select * +
                  From K_Dlyprod +
                  Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr clsr1cursor;
       exec sql
         close r1cursor;
       endsr;

      /end-free
