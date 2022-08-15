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
      **   Name: K3S_3205
      **   Type: SQLRPGLE program
      **   Desc: Read K_PRODSED (detail) records and write k_PRODSEB
      **         (header) record for batch for Core-Mark.
      *********************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 07/02/2021.
      *   Remarks.
      *
      *********************************************************************
      * ----------------------------------------------------------
     d prodsed_rec   e ds                  ExtName(k_prodsed)
      * ----------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------- work fields
     d hcblank1        s              1    inz(' ')
     d hcblank5        s              5    inz('     ')
     d hcblank10       s             10    inz('          ')
     d hcrequser       s             10    inz('NIGHT_JOB ')
     d hcactbuyr       s              5    inz('    ')
     d hcactuser       s             10    inz('KING3')
     d hcreqtype       s              3a   inz('   ')
     d hcreqtype11     c                   const('Q11')
     d hcreqtype12     c                   const('Q12')
     d hcreqtype13     c                   const('Q13')
     d hcstatus        s              1    inz('O')
     d hcdescr         s             30a   inz('                              ')
     d hcdescr11       c                   'Averages adjusted at divisions'
     d hcdescr12       c                   'Products with reduced averages'
     d hcdescr13       c                   'Products with increase average'
     d hccrtuser       s             10    inz('K3S_3205')
     d hcpgmtype       s             10    inz('H')
     d codeval         s              3    inz('  ')
     d codetyp         s              3    inz('SPR')
     d pdcount         s              6  0 inz(0)
     d pdupdcount      s              6  0 inz(0)
     d current_date    s               d
     d cmsysdate       s               d
     d zero            s              1  0 inz(0)
     d last_batch#     s              7  0 inz(0)
     d new_batch#      s              7  0 inz(0)
     d hctime          s               t   timfmt(*hms:)
     d
      * --------------------------------------------------- prototype
     d K3S_3205        PR                  EXTPGM('K3S_3205')
     d  comp                          1
      * --------------------------------------------------- procedure interface
     d K3S_3205        PI
     d  comp                          1
      * ---------------------------------------------------------- Begin
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endmod;

       exec sql
          select cm_sysdate
             into :cmsysdate   ---host variable to hold date from k_company---
             from k_company
             where cm_comp = :comp ---select k_company row based on parm passed---
             fetch first row only;

       hcreqtype = hcreqtype11;   //Read K_PRODSED for pd_reqty[ = 'Q11'
       hcdescr = *blanks;

       exec sql
         select ta_codeds1
            into :hcdescr
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = :codetyp and
                  ta_codeval = :hcreqtype
            fetch first row only;

       exec sql
         select count(*)
            into :pdcount
            from k_prodsed
            where pd_comp = :comp and
                  pd_reqtype = :hcreqtype and
                  pd_batch = 0 and
                  pd_birth >= :cmsysdate;

       if pdcount > 0;
       //K_PRODSED records with specified pd_reqtype exist

          current_date = %date();

          exec sql
             select cm_lastba
                  into :last_batch#   --Get last batch # used--
                  from k_company
                  where cm_comp = :comp;

          if SQLState = SQLStateOk;

             //Increment last_batch# used to get new batch#
             if last_batch# + 1 > 9999999;
                last_batch# = 0;
             endif;
             new_batch# = last_batch# + 1;

             exec sql
                update k_company
                   set cm_lastba = :new_batch#   --Update last batch # --
                   where cm_comp = :comp;

             if SQLState = SQLStateOk;
                exsr dclpdcursor;  //declare cursor
                exsr opnpdcursor;  //open cursor

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
                dow SQLSTT = SQLStateOk;

        //fetch k_prodfor row
                   exec sql
                     fetch next
                     from pdcursor
                     into :prodsed_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;

                   exec sql
                      update k_prodsed
                         set pd_batch = :new_batch#
                         where current of pdcursor;

                   if SQLState = SQLStateOk;
                      pdupdcount += 1;
                   endif;

                   SQLState = SQLStateOk;
                enddo;
                exsr clspdcursor;
                exsr insert_prodseb;
             endif;
          endif;
       endif;

       pdupdcount = 0;
       hcreqtype = hcreqtype12;   //Read K_PRODSED for pd_reqtyp = 'Q12'
       hcdescr   = *blanks;

       exec sql
          select ta_codeds1
             into :hcdescr
             from k_tablcod
             where ta_comp = :comp and
                   ta_codetyp = :codetyp and
                   ta_codeval = :hcreqtype
             fetch first row only;

       exec sql
         select count(*)
            into :pdcount
            from k_prodsed
            where pd_comp = :comp and
                  pd_reqtype = :hcreqtype and
                  pd_batch = 0 and
                  pd_birth >= :cmsysdate;

       if pdcount > 0;
       //K_PRODSED records with specified pd_reqtype exist

          current_date = %date();

          exec sql
             select cm_lastba
                  into :last_batch#   --Get last batch # used--
                  from k_company
                  where cm_comp = :comp;

          if SQLState = SQLStateOk;

             //Increment last_batch# used to get new batch#
             if last_batch# + 1 > 9999999;
                last_batch# = 0;
             endif;
             new_batch# = last_batch# + 1;

             exec sql
                update k_company
                   set cm_lastba = :new_batch#   --Update last batch # --
                   where cm_comp = :comp;

             if SQLState = SQLStateOk;
           //   exsr dclpdcursor;
                exsr opnpdcursor;  //open cursor

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
                dow SQLSTT = SQLStateOk;

        //fetch k_prodfor row
                   exec sql
                     fetch next
                     from pdcursor
                     into :prodsed_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;

                   exec sql
                      update k_prodsed
                         set pd_batch = :new_batch#
                         where current of pdcursor;

                   if SQLState = SQLStateOk;
                      pdupdcount += 1;
                   endif;

                   SQLState = SQLStateOk;
                enddo;
                exsr clspdcursor;
                exsr insert_prodseb;
             endif;
          endif;
       endif;

       pdupdcount = 0;
       hcreqtype = hcreqtype13;   //Read K_PRODSED for pd_reqtyp = 'Q11'
       hcdescr   = *blanks;

       exec sql
          select ta_codeds1
             into :hcdescr
             from k_tablcod
             where ta_comp = :comp and
                   ta_codetyp = :codetyp and
                   ta_codeval = :hcreqtype
             fetch first row only;

       exec sql
         select count(*)
            into :pdcount
            from k_prodsed
            where pd_comp = :comp and
                  pd_reqtype = :hcreqtype and
                  pd_batch = 0 and
                  pd_birth >= :cmsysdate;

       if pdcount > 0;
       //K_PRODSED records with specified pd_reqtype exist

          current_date = %date();

          exec sql
             select cm_lastba
                  into :last_batch#   --Get last batch # used--
                  from k_company
                  where cm_comp = :comp;

          if SQLState = SQLStateOk;

             //Increment last_batch# used to get new batch#
             if last_batch# + 1 > 9999999;
                last_batch# = 0;
             endif;
             new_batch# = last_batch# + 1;

             exec sql
                update k_company
                   set cm_lastba = :new_batch#   --Update last batch # --
                   where cm_comp = :comp;

             if SQLState = SQLStateOk;
           //   exsr dclpdcursor;
                exsr opnpdcursor;  //open cursor

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
                dow SQLSTT = SQLStateOk;

        //fetch k_prodfor row
                   exec sql
                     fetch next
                     from pdcursor
                     into :prodsed_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;

                   exec sql
                      update k_prodsed
                         set pd_batch = :new_batch#
                         where current of pdcursor;

                   if SQLState = SQLStateOk;
                      pdupdcount += 1;
                   endif;

                   SQLState = SQLStateOk;
                enddo;
                exsr clspdcursor;
                exsr insert_prodseb;
             endif;
          endif;
       endif;

       *inlr = *on;

       begsr dclpdcursor;
       exec sql
        declare pdcursor Cursor
         for
         select *  ---select all columns in k_prodsed rows---
         from k_prodsed
         where pd_comp = :comp and
               pd_reqtype = :hcreqtype and
               pd_batch = 0 and
               pd_birth >= :cmsysdate;
       endsr;

       begsr opnpdcursor;
       exec sql
         open pdcursor;
       endsr;

       begsr clspdcursor;
       exec sql
         close pdcursor;
       endsr;

       begsr insert_prodseb;

       exec sql
          insert into k_prodseb   --Write batch header record --
              (pb_comp,
               pb_requser,
               pb_reqregn,
               pb_reqlocn,
               pb_reqbuyr,
               pb_reqsupl,
               pb_reqsub,
               pb_reqtype,
               pb_batch,
               pb_birth,
               pb_lastupd,
               pb_actbuyr,
               pb_actregn,
               pb_actlocn,
               pb_actuser,
               pb_actsupl,
               pb_actsub,
               pb_status,
               pb_descr,
               pb_prodsel,
               pb_prodrem,
               pb_produpd,
               pb_pgmtype,
               pb_bchbusy,
               pb_lckuser,
               pb_lcktime,
               pb_sepbuyr,
               pb_sepregn,
               pb_seplocn,
               pb_sepsupl,
               pb_mindolr,
               pb_minunit,
               pb_suplgp1,
               pb_suplgp2,
               pb_suplgp3,
               pb_suplgp4,
               pb_suplgp5,
               pb_prodgp1,
               pb_prodgp2,
               pb_prodgp3,
               pb_prodgp4,
               pb_prodgp5,
               pb_hidevp,
               pb_szhilo,
               pb_szsens,
               pb_szma12,
               pb_szma13,
               pb_selview,
               pb_forcint,
               pb_usrfroz,
               pb_usrmanl,
               pb_usrprob,
               pb_usrwatc,
               pb_usrnone,
               pb_sysdisc,
               pb_syslump,
               pb_sysnew,
               pb_sysregl,
               pb_sysslow,
               pb_crtuser)
          values
              (:comp,
               :hcrequser,
               :hcblank5,
               :hcblank5,
               :hcblank5,
               :hcblank10,
               :hcblank10,
               :hcreqtype,
               :new_batch#,            --new batch# --
               :current_date,
               :current_date,
               :hcactbuyr,
               :hcblank5,
               :hcblank5,
               :hcactuser,
               :hcblank10,
               :hcblank10,
               :hcstatus,
               :hcdescr,
               :pdupdcount,
               :zero,
               :zero,
               :hcpgmtype,
               :zero,
               :hcblank10,
               :hctime,
               :zero,
               :zero,
               :zero,
               :zero,
               :zero,
               :zero,
               :hcblank10,
               :hcblank10,
               :hcblank10,
               :hcblank10,
               :hcblank10,
               :hcblank10,
               :hcblank10,
               :hcblank10,
               :hcblank10,
               :hcblank10,
               :zero,
               :zero,
               :zero,
               :zero,
               :zero,
               :zero,
               :zero,
               :hcblank1,
               :hcblank1,
               :hcblank1,
               :hcblank1,
               :hcblank1,
               :hcblank1,
               :hcblank1,
               :hcblank1,
               :hcblank1,
               :hcblank1,
               :hccrtuser);
       endsr;
       /end-free
