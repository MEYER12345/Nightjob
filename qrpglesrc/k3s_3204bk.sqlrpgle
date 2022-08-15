      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO)
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
      **   Name: K3S_3204   (this source cloned from K3S_3201 which is retired)
      **   Type: SQLRPGLE program
      **   Desc: Read K_PRODFOR J recs, write K recs, update K_PRODUCT pr_forcast
      **
      *********************************************************************
      *
      *   2021-07-04 King3 - Changed default cursor pfcursor to include
      *                      test on pf_birth = cmsysdate
      *
      *********************************************************************
      *
      *   2021-07-05 King3 - David helped me understand to put the : in
      *                      front of the cmsysdate
      *                    - Now I don't need one of the IF, ENDIF sets
      *
      *********************************************************************
      *
      *   2021-07-07 King3 - David helped me to make K_PRODUCT updatable
      *
      *********************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 02/28/2022.
      *   Remarks. After K3's testing of 'Hub and Spoke' programs he
      *      requested that I make the following adjustments to this
      *      program:
      *         1) Change dclpfcursor routine to look for pf_chgtype of
      *            'J' instead of 'X'.
      *         2) Write out K_PRODFOR records with pf_chgtype of 'K'
      *            instead of 'N'.
      *********************************************************************
     d prodfor_rec   e ds                  ExtName(k_prodfor)
      * ----------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------- work fields
     d cmsysdate       s               d
     d birthtm         s               t
     d prforcast       s              9  2
     d prfordevp       s              3  1
     d prseasonl       s             10
     d prusrstat       s              1
     d prsysstat       s              1
     d prprodseq       s             40a
     d prdesc1         s             40a
     d prsupl          s             10a
     d prsuplsub       s             10a
     d prsuplusr       s             10a
     d prsuplusb       s             10a
     d prbuyr          s              5
     d prregn          s              5
     d pransale$       s             13  2
     d pransaleu       s             13  2
     d mm              s              2  0
     d mm_alpha        s              2
     d dd              s              2  0
     d dd_alpha        s              2
     d min             s              2  0
     d min_alpha       s              2
     d sec             s              2  0
     d sec_alpha       s              2
     d hrs             s              2  0
     d hrs_alpha       s              2
     d same_prod       s             25
     d locn            s              3
     d supl            s             10
     d suplsub         s             10
     d suplusr         s             10
     d suplusb         s             10
     d zero            s              1  0 inz(0)
     d current_date    s               d
     d end_date        s               d
     d time_stamp      s               z
     d hld_qty         s              7  0
     d po#             s             10
     d source          s              1
     d hcuser          s             10    inz('KING3     ')
     d reqtype         s              3    inz('   ')
     d program         s             10    inz('K3S_3204')
     d chgtype         s             10    inz('A')
     d workstn         s             10    inz('K3SNiteJob')
     d codetyp         s              3a   inz('HAS')
     d codeval         s             20a
     d hub_locn        s              8a   inz('HUB LOCN')
     d flag3_count     s              5s 0
     d taflag3         s              1s 0
     d taflag4         s              1s 0
     d bypass_logic    s              1a
      * --------------------------------------------------- prototype
     d K3S_3204        PR                  EXTPGM('K3S_3204')
     d  comp                          1
     d  passDATE                     10
      * --------------------------------------------------- procedure interface
     d K3S_3204        PI
     d  comp                          1
     d  passDATE                     10
      * ---------------------------------------------------------- Begin
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endmod;

       bypass_logic = 'Y';

       exec SQL
           select count(*)         ---Loop thru HAS k_tablcod rows---
              into :flag3_count    ---If no ta_flag3 = 1 bypass main logic---
              from k_tablcod
              where ta_comp = :comp and
                    ta_codetyp = :codetyp and
                    ta_flag3 = 1;
       if flag3_count > 0;
          bypass_logic = 'N';
       endif;

       if bypass_logic = 'N';

          exec sql
             select cm_sysdate
                into :cmsysdate   ---host variable to hold date from k_company---
                from k_company
                where cm_comp = :comp ---select k_company row based on parm passed---
                fetch first row only;

             if passDATE <> *blanks;
             cmsysdate = %date(passDATE);
             endif;

          exsr dclpfcursor;  //declare cursor
          exsr opnpfcursor;  //open cursor

          birthtm = %time();

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
          dow SQLSTT = SQLStateOk;

        //fetch k_prodfor row
             exec sql
              fetch next
              from pfcursor
              into :prodfor_rec;

             if SQLState = RowNotFound;
                leave;
             endif;

             codeval = *blanks;
             codeval = hub_locn + '  ' + pf_locn;

             exec SQL
                select ta_flag3, ta_flag4
                   into :taflag3, :taflag4
                   from k_tablcod
                      where ta_comp = :comp and
                            ta_codetyp = :codetyp and
                            ta_codeval = :codeval
                      fetch first row only;

             if SQLState = SQLStateOk and
                taflag3 = 1;

             // cursor providing all the records to be processed
                pf_chgtype = 'K';

                exec sql
             ---select pr_forcast, pr_fordevp, pr_seasonl, pr_usrstat, pr_sysstat ---
             ---   into :prforcast, :prfordevp, :prseasonl, :prusrstat, :prsysstat ---
                select pr_forcast, pr_fordevp, pr_seasonl, pr_usrstat,
                       pr_sysstat
                   into :prforcast, :prfordevp, :prseasonl, :prusrstat,
                        :prsysstat
                      from k_product
                      where pr_comp = :comp and
                            pr_locn = :pf_locn and
                            pr_supl = :pf_supl and
                            pr_suplsub = :pf_suplsub and
                            pr_prod = :pf_prod
                      fetch first row only;

                if SQLState = SQLStateOk;
                   mm = %subdt(pf_birth:*months);
                   mm_alpha = %editc(mm:'X');
                   dd = %subdt(pf_birth:*days);
                   dd_alpha = %editc(dd:'X');
                   hrs= %subdt(pf_birthtm:*hours);
                   hrs_alpha= %editc(hrs:'X');
                   min= %subdt(pf_birthtm:*minutes);
                   min_alpha= %editc(min:'X');
                   sec= %subdt(pf_birthtm:*seconds);
                   sec_alpha= %editc(sec:'X');
                   pf_chgdesc = 'L=' + %subst(pf_chgdesc:10:3) + ' ' +
                                       mm_alpha + '/' + dd_alpha + '-' +
                                           hrs_alpha + ':' +
                                           min_alpha + ':' +
                                           sec_alpha;
                   pf_birth   = %date();

                   if pf_prod = same_prod;
                      birthtm = birthtm + %seconds(01);
                   endif;
                   same_prod = pf_prod;
                   pf_birthtm = birthtm;

                   pf_program = program;
                   pf_avgbef  = prforcast;
                   pf_avgaft  = prforcast + pf_avgdiff;

                   if pf_avgaft < 0;
                      pf_avgaft = 0;
                      pf_avgdiff = prforcast * -1;
                   endif;

                   pf_devpbef = prfordevp;
                   pf_devpaft = prfordevp;
                   pf_seasbef = prseasonl;
                   pf_seasaft = prseasonl;
                   pf_statbef = prusrstat;
                   pf_stataft = prusrstat;
                   pf_sysstat = prsysstat;

                   exec sql
                      update k_product
                      set pr_forcast = pr_forcast + :pf_avgdiff
                      where pr_comp = :pf_comp and
                            pr_locn = :pf_locn and
                            pr_supl = :pf_supl and
                            pr_suplsub = :pf_suplsub and
                            pr_prod = :pf_prod;

                   if SQLState = SQLStateOk;

                      exec sql
                         insert into k_prodfor
                         values (:prodfor_rec);

                      if SQLState = SQLStateOk;

                   //Write record to k_prodhld if pf_avgdiff > 0
                         if pf_avgdiff > 0;
                            locn = %subst(pf_chgdesc:3:3);
                            current_date = %date();
                            end_date = current_date + %days(14);
                            time_stamp = %timestamp();
                            hld_qty = %int(pf_avgdiff);
                            if hld_qty < 1;
                               hld_qty = 1;
                            endif;
                            po# = *blanks;
                            source = 'X';

                            exec sql
                               select pr_supl, pr_suplsub, pr_suplusr,
                                      pr_suplusb, pr_prodseq, pr_desc1,
                                      pr_buyr, pr_ansale$, pr_ansaleu,
                                       pr_regn
                               into :prsupl, :prsuplsub, :prsuplusr,
                                    :prsuplusb, :prprodseq, :prdesc1,
                                    :prbuyr, :pransale$, :pransaleu,
                                    :prregn
                               from k_product
                               where pr_comp = :comp and
                                     pr_locn = :locn and
                                     pr_prod = :pf_prod and
                                     pr_altsrce = 0 and        --regular source
                                     pr_deltcnt = 0 and        --delete count
                                     pr_tempory = 0            --temporary record
                               fetch first row only;

                            if SQLState = SQLStateOk;
                               exsr insert_prodhld;
                               if SQLState = SQLStateOk;
                                  reqtype = 'Q11';         //Any adjustment (up or down)
                                  exsr insert_prodsed;
                                  if SQLState = SQLStateOk;
                                     reqtype = 'Q13';      //For positive adjustments
                                     exsr insert_prodsed;
                                     if SQLState = SQLStateOk and taflag4 = 1;
                                        exec sql
                                           delete k_prodfor
                                              where current of pfcursor;
                                     endif;
                                  endif;
                               endif;
                            endif;
                         endif;

               //Write record to k_prodsed if pf_avgdiff < 0
                         if pf_avgdiff < 0;

                            locn = %subst(pf_chgdesc:3:3);

                            exec sql
                               select pr_supl, pr_suplsub, pr_buyr, pr_regn,
                                      pr_prodseq, pr_desc1, pr_ansale$,
                                      pr_ansaleu
                                 into :prsupl, :prsuplsub, :prbuyr, :prregn,
                                      :prprodseq, :prdesc1, :pransale$,
                                      :pransaleu
                                 from k_product
                                 where pr_comp = :comp and
                                      pr_locn = :locn and
                                      pr_prod = :pf_prod and
                                      pr_altsrce = 0 and        --regular source
                                      pr_deltcnt = 0 and        --delete count
                                      pr_tempory = 0            --temporary record
                                 fetch first row only;

                            if SQLState = SQLStateOK;
                               current_date = %date();
                               reqtype = 'Q11';                  //Any adjustment (up or down)
                               exsr insert_prodsed;
                               if SQLState = SQLStateOK;
                                  reqtype = 'Q12';               //Reduced averages
                                  exsr insert_prodsed;
                                  if SQLState = SQLStateOk and taflag4 = 1;
                                     exec SQL
                                        delete k_prodfor
                                           where current of pfcursor;
                                  endif;
                             endif;
                            endif;
                         endif;
                      endif;
                   endif;
                endif;
             endif;
             SQLState = SQLStateOk;
          enddo;
          exsr clspfcursor;

       endif;

       *inlr = *on;

       begsr dclpfcursor;
       exec sql
        declare pfcursor Cursor
         for
         select *  ---select all columns in k_prodfor rows---
         from k_prodfor
         where pf_comp = :comp and
               pf_birth >= :cmsysdate and
               pf_chgtype = 'J'
               order by pf_comp,
                        pf_locn,
                        pf_supl,
                        pf_suplsub,
                        pf_prod,
                        pf_birth,
                        pf_birthtm;
       endsr;

       begsr opnpfcursor;
       exec sql
         open pfcursor;
       endsr;

       begsr clspfcursor;
       exec sql
         close pfcursor;
       endsr;

       begsr insert_prodhld;

       exec sql
          insert into k_prodhld   --Write product hold out qty record
              (pu_comp,
               pu_locn,
               pu_supl,
               pu_suplsub,
               pu_suplusr,
               pu_suplusb,
               pu_prod,
               pu_birth,
               pu_lastupd,
               pu_begin,
               pu_end,
               pu_hldqty,
               pu_hldreas,
               pu_timestp,
               pu_po#,
               pu_source)
          values
              (:comp,
               :locn,
               :prsupl,
               :prsuplsub,
               :prsuplusr,
               :prsuplusb,
               :pf_prod,
               :current_date,
               :current_date,
               :current_date,
               :end_date,
               :hld_qty,
               :pf_chgdesc,
               :time_stamp,
               :po#,
               :source);
       if SQLState = SQLStateOk;
          exec sql
             insert into k_phldlog   --Write log record
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
          values
               (:comp,
                :chgtype,
                :current_date,
                :birthtm,
                :pf_user,
                :workstn,
                :program,
                :locn,
                :prsupl,
                :prsuplsub,
                :current_date,
                :end_date,
                :pf_chgdesc,
                :pf_prod,
                :hld_qty,
                :po#);
       endif;
       endsr;


       begsr insert_prodsed;

       exec sql
          insert into k_prodsed   --Write batch detail record
              (pd_comp,
               pd_reqtype,
               pd_batch,
               pd_birth,
               pd_lastupd,
               pd_actbuyr,
               pd_actregn,
               pd_actlocn,
               pd_actuser,
               pd_actsupl,
               pd_actsub,
               pd_prod,
               pd_prodseq,
               pd_desc1,
               pd_review,
               pd_ansale$,
               pd_ansaleu)
          values
              (:comp,
               :reqtype,
               :zero,
               :current_date,
               :current_date,
               :prbuyr,
               :prregn,
               :locn,
               :hcuser,
               :prsupl,
               :prsuplsub,
               :pf_prod,
               :prprodseq,
               :prdesc1,
               :zero,
               :pransale$,
               :pransaleu);
        endsr;
       /end-free
