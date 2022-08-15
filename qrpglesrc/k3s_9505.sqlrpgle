      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

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
      **   Name: K3S_9505
      **   Type: ILE RPG Program
      **   Desc: Check records for deletion based up days
      **
      *****************************************************************
      **
      **  This program is used to check certain files, and see if
      **  records within these files can be deleted.
      **  The decision is based upon the number of days value that
      **  comes from the tables code file.
      **  Each file to be checked in this program has a corresponding
      **  table code record that contains the days value to be used.
      **
      * ***************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/15/2016.
      **  Remarks. Converted program to free format RPG with embedded SQL.
      **
      * -------------------------------------------------------- work fields
     d diff_days       s              7  0
     d tanumber1       s              5  0
     d year4           s              4  0
     d date10          s             10
     d userID          s             10
     d test_date       s               d   inz(d'0001-01-01') datfmt(*iso)
     d cmsysdate       s               d   datfmt(*iso)                         system date (*ISO)
     d tr_birth        s               d   datfmt(*iso)
     d MsgText         s            256
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
     d CrsrAlreadyOpn  c                   Const('24502')
      * -------------------------------------------------------
     d dealsum_rec   e ds                  ExtName(k_dealsum)
     d deallog_rec   e ds                  ExtName(k_deallog)
     d hstaltr_rec   e ds                  ExtName(k_hstaltr)
     d intcsti_rec   e ds                  ExtName(k_intcsti)
     d intcstj_rec   e ds                  ExtName(k_intcstj)
     d intinpt_rec   e ds                  ExtName(k_intinpt)
     d logprod_rec   e ds                  ExtName(k_logprod)
     d logaltr_rec   e ds                  ExtName(k_logaltr)
     d prodovr_rec   e ds                  ExtName(k_prodovr)
     d guestbk_rec   e ds                  ExtName(k_guestbk)
     d buyrsot_rec   e ds                  ExtName(k_buyrsot)
     d custbch_rec   e ds                  ExtName(k_custbch)
     d custprd_rec   e ds                  ExtName(k_custprd)
     d intordb_rec   e ds                  ExtName(k_intordb)
     d intordd_rec   e ds                  ExtName(k_intordd)
     d notepad_rec   e ds                  ExtName(k_notepad)
     d prodfor_rec   e ds                  ExtName(k_prodfor)
     d prodhis_rec   e ds                  ExtName(k_prodhis)
     d prodhld_rec   e ds                  ExtName(k_prodhld)
     d prodh52_rec   e ds                  ExtName(k_prodh52)
     d prodltm_rec   e ds                  ExtName(k_prodltm)
     d prodseb_rec   e ds                  ExtName(k_prodseb)
     d prodsed_rec   e ds                  ExtName(k_prodsed)
     d purchis_rec   e ds                  ExtName(k_purchis)
     d pwkdlog_rec   e ds                  ExtName(k_pwkdlog)
     d scheddy_rec   e ds                  ExtName(k_scheddy)
     d schedpe_rec   e ds                  ExtName(k_schedpe)
     d suplltm_rec   e ds                  ExtName(k_suplltm)
     d suplpur_rec   e ds                  ExtName(k_suplpur)
     d suplrej_rec   e ds                  ExtName(k_suplrej)
     d buyrlog_rec   e ds                  ExtName(k_buyrlog)
     d complog_rec   e ds                  ExtName(k_complog)
     d locnlog_rec   e ds                  ExtName(k_locnlog)
     d phldlog_rec   e ds                  ExtName(k_phldlog)
     d prodlog_rec   e ds                  ExtName(k_prodlog)
     d supllog_rec   e ds                  ExtName(k_supllog)
     d tabllog_rec   e ds                  ExtName(k_tabllog)
     d userlog_rec   e ds                  ExtName(k_userlog)
     d supdlog_rec   e ds                  ExtName(k_supdlog)
     d svoclog_rec   e ds                  ExtName(k_svoclog)
     d autolog_rec   e ds                  ExtName(k_autolog)
     d plnklog_rec   e ds                  ExtName(k_plnklog)
     d psrvlog_rec   e ds                  ExtName(k_psrvlog)
     d scdylog_rec   e ds                  ExtName(k_scdylog)
     d scpelog_rec   e ds                  ExtName(k_scpelog)
     d tcenlog_rec   e ds                  ExtName(k_tcenlog)
     d tspllog_rec   e ds                  ExtName(k_tspllog)
     d tprdlog_rec   e ds                  ExtName(k_tprdlog)
     d dperlog_rec   e ds                  ExtName(k_dperlog)
     d bsrvlog_rec   e ds                  ExtName(k_bsrvlog)
     d gsrvlog_rec   e ds                  ExtName(k_gsrvlog)
     d notelog_rec   e ds                  ExtName(k_notelog)
     d intdaly_rec   e ds                  ExtName(k_intdaly)
     d dlyprod_rec   e ds                  ExtName(k_dlyprod)
     d prodapp_rec   e ds                  ExtName(k_prodapp)
     d weekdis_rec   e ds                  ExtName(k_weekdis)
     d intdeal_rec   e ds                  ExtName(k_intdeal)
     d invprod_rec   e ds                  ExtName(k_invprod)
     d invsupl_rec   e ds                  ExtName(k_invsupl)
     d dlysvce_rec   e ds                  ExtName(k_dlysvce)
     d perdper_rec   e ds                  ExtName(k_perdper)
      * --------------------------------------------------- parameter passed prototype
      /copy k3s_proto
      * --------------------------------------------------  procedure interface
     d K3S_9505        PI
     d  comp                          1
      /free

       // ---------------------------------------------------------- Key Lists
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endmod;

       //   get today's date
        exec sql
          select cm_sysdate
            into :cmsysdate
            from k_company
            where cm_comp = :comp
            fetch first row only;

       // ---------------------------------------------------------- k_dealsum
       // check k_dealsum file, and get k_dealdet + k_dealalw too
        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_DEALSUM '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcldmcursor_9505;
              exsr opendmcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from dmcursor_9505
                   into :dealsum_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:dm_end:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
       // call module to delete deal
                      userID = 'K3S_DLTREC';
                      callp K3S_5110(dm_comp:
                                     dm_locn:
                                     dm_deal:
                                     userID);
                   endif;
              enddo;
              exsr clsdmcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_deallog
       // check k_deallog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_DEALLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;
       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcldgcursor_9505;
              exsr opendgcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from dgcursor_9505
                   into :deallog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:dg_timestp:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_deallog
                          where current of dgcursor_9505;
                   endif;
              enddo;
              exsr clsdgcursor_9505;
           endif;
        endif;
       // ---------------------------------------------------------- k_hstaltr
       // check k_hstaltr file
        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_HSTALTR '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclhacursor_9505;
              exsr openhacursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from hacursor_9505
                   into :hstaltr_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;

                   if SQLState = SQLStateOk;
                      diff_days = %diff(cmsysdate:ha_birth:*days);
                      if diff_days > tanumber1 AND
                         diff_days > 60;
                         exec sql
                           delete from k_hstaltr
                             where current of hacursor_9505;
                      endif;
                   endif;
              enddo;
              exsr clshacursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_intcsti
       // check k_intcsti file
        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INTCSTI '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcliicursor_9505;
              exsr openiicursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from iicursor_9505
                   into :intcsti_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:ii_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_intcsti
                          where current of iicursor_9505;
                   endif;
              enddo;
              exsr clsiicursor_9505;
           endif;
        endif;
       // ---------------------------------------------------------- k_intcstj
       // check k_intcstj file
        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INTCSTJ '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclijcursor_9505;
              exsr openijcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from ijcursor_9505
                   into :intcstj_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:ij_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_intcstj
                          where current of ijcursor_9505;
                   endif;
              enddo;
              exsr clsijcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_intinpt
       // check k_intinpt file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INTINPT '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcls8cursor_9505;
              exsr opens8cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from s8cursor_9505
                   into :intinpt_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:s8_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_intinpt
                          where current of s8cursor_9505;
                   endif;
              enddo;
              exsr clss8cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_logprod
       // check k_logprod file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_LOGPROD '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcllgcursor_9505;
              exsr openlgcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from lgcursor_9505
                   into :logprod_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   test_date = %date(%subst(%char(lg_timestp):1:10):*ISO);
                   diff_days = %diff(cmsysdate:test_date:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_logprod
                          where current of lgcursor_9505;
                   endif;
              enddo;
              exsr clslgcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_logaltr
       // check k_logaltr file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_LOGALTR '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcllacursor_9505;
              exsr openlacursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from lacursor_9505
                   into :logaltr_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:la_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_logaltr
                          where current of lacursor_9505;
                   endif;
              enddo;
              exsr clslacursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_guestbk
       // check k_guestbk file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_GUESTBK '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclgbcursor_9505;
              exsr opengbcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from gbcursor_9505
                   into :guestbk_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   test_date = %date(%subst(%char(gb_timestr):1:10):*ISO);
                   diff_days = %diff(cmsysdate:test_date:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_guestbk
                          where current of gbcursor_9505;
                   endif;
              enddo;
              exsr clsgbcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodovr
       // check k_prodovr file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODOVR '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclpocursor_9505;
              exsr openpocursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from pocursor_9505
                   into :prodovr_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:po_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodovr
                          where current of pocursor_9505;
                   endif;
              enddo;
              exsr clspocursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_buyrsot
       // check k_buyrsot file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_BUYRSOT '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclbtcursor_9505;
              exsr openbtcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from btcursor_9505
                   into :buyrsot_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:bt_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_buyrsot
                          where current of btcursor_9505;
                   endif;
               enddo;
               exsr clsbtcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_custbch
       // check k_custbch file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_CUSTBCH '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclcbcursor_9505;
              exsr opencbcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from cbcursor_9505
                   into :custbch_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:cb_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_custbch
                           where current of cbcursor_9505;
                   endif;
              enddo;
              exsr clscbcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_custprd
       // check k_custprd file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_CUSTPRD '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclcpcursor_9505;
              exsr opencpcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from cpcursor_9505
                   into :custprd_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:cp_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_custprd
                           where current of cpcursor_9505;
                   endif;
              enddo;
              exsr clscpcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_intordb
       // check k_intordb file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INTORDB '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclibcursor_9505;
              exsr openibcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from ibcursor_9505
                   into :intordb_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:ib_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_intordb
                           where current of ibcursor_9505;
                   endif;
              enddo;
              exsr clsibcursor_9505;
           endif;
        endif;
       // ---------------------------------------------------------- k_intordd
       // check k_intordd file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INTORDD '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclidcursor_9505;
              exsr openidcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from idcursor_9505
                   into :intordd_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:id_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_intordd
                          where current of idcursor_9505;
                   endif;
              enddo;
              exsr clsidcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_notepad
       // check k_notepad file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_NOTEPAD '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclntcursor_9505;
              exsr openntcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from ntcursor_9505
                   into :notepad_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:nt_birth:*days);

                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_notepad
                          where current of ntcursor_9505;
                   endif;
              enddo;
              exsr clsntcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodfor
       // check k_prodfor file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODFOR '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclpfcursor_9505;
              exsr openpfcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from pfcursor_9505
                   into :prodfor_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:pf_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodfor
                          where current of pfcursor_9505;
                   endif;
              enddo;
              exsr clspfcursor_9505;
           endif;
        endif;
       // ---------------------------------------------------------- k_prodhis
       // check k_prodhis file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODHIS '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclphcursor_9505;
              exsr openphcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from phcursor_9505
                   into :prodhis_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;

                   year4 = %dec(%subst(%editc(ph_year:'X'):1:4):4:0);
                   date10 = %editc(year4:'X');
                   %subst(date10:5:6) = '-01-01';
                   test_date = %date(date10);

                   diff_days = %diff(cmsysdate:test_date:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodhis
                          where current of phcursor_9505;
                   endif;
              enddo;
              exsr clsphcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodhld
       // check k_prodhld file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODHLD '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclpucursor_9505;
              exsr openpucursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from pucursor_9505
                   into :prodhld_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:pu_end:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodhld
                          where current of pucursor_9505;
                   endif;
              enddo;
              exsr clspucursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodh52
       // check k_prodh52 file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODH52 '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclpwcursor_9505;
              exsr openpwcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from pwcursor_9505
                   into :prodh52_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   year4 = %dec(%subst(%editc(pw_year:'X'):1:4):4:0);
                   date10 = %editc(year4:'X');
                   %subst(date10:5:6) = '-01-01';
                   test_date = %date(date10);
                   diff_days = %diff(cmsysdate:test_date:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodh52
                          where current of pwcursor_9505;
                   endif;
              enddo;
              exsr clspwcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodltm
       // check k_prodltm file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODLTM '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclplcursor_9505;
              exsr openplcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from plcursor_9505
                   into :prodltm_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:pl_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodltm
                          where current of plcursor_9505;
                   endif;
              enddo;
              exsr clsplcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodseb
       // check k_prodseb file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODSEB '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclpbcursor_9505;
              exsr openpbcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from pbcursor_9505
                   into :prodseb_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:pb_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodseb
                          where current of pbcursor_9505;
                   endif;
              enddo;
              exsr clspbcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodsed
       // check k_prodsed file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODSED '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclpdcursor_9505;
              exsr openpdcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from pdcursor_9505
                   into :prodsed_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:pd_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodsed
                          where current of pdcursor_9505;
                   endif;
              enddo;
              exsr clspdcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_purchis
       // check k_purchis file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PURCHIS '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclpscursor_9505;
              exsr openpscursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from pscursor_9505
                   into :purchis_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:ps_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_purchis
                          where current of pscursor_9505;
                   endif;
              enddo;
              exsr clspscursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_pwkdlog
       // check k_pwkdlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PWKDLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclz0cursor_9505;
              exsr openz0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from z0cursor_9505
                   into :pwkdlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:z0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_pwkdlog
                          where current of z0cursor_9505;
                   endif;
              enddo;
              exsr clsz0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_scheddy
       // check k_scheddy file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SCHEDDY '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclsycursor_9505;
              exsr opensycursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from sycursor_9505
                   into :scheddy_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:sy_sysdate:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_scheddy
                          where current of sycursor_9505;
                   endif;
              enddo;
              exsr clssycursor_9505;
           endif;
       endif;

       // ---------------------------------------------------------- k_schedpe
       // check k_schedpe file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SCHEDPE '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclsecursor_9505;
              exsr opensecursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from secursor_9505
                   into :schedpe_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:se_ending:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_schedpe
                          where current of secursor_9505;
                   endif;
              enddo;
              exsr clssecursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_suplltm
       // check k_suplltm file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SUPLLTM '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclslcursor_9505;
              exsr openslcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from slcursor_9505
                   into :suplltm_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:sl_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_suplltm
                          where current of slcursor_9505;
                   endif;
              enddo;
              exsr clsslcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_suplpur
       // check k_suplpur file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SUPLPUR '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclsacursor_9505;
              exsr opensacursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from sacursor_9505
                   into :suplpur_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:sa_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_suplpur
                          where current of sacursor_9505;
                   endif;
              enddo;
              exsr clssacursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_suplrej
       // check k_suplrej file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SUPLREJ '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclsrcursor_9505;
              exsr opensrcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from srcursor_9505
                   into :suplrej_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:sr_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_suplrej
                          where current of srcursor_9505;
                   endif;
              enddo;
              exsr clssrcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_buyrlog
       // check k_buyrlog file
        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_BUYRLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclb0cursor_9505;
              exsr openb0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from b0cursor_9505
                   into :buyrlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:b0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_buyrlog
                          where current of b0cursor_9505;
                   endif;
              enddo;
              exsr clsb0cursor_9505;
           endif;

        endif;

       // ---------------------------------------------------------- k_complog
       // check k_complog file

       exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_COMPLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclc0cursor_9505;
              exsr openc0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from c0cursor_9505
                   into :complog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:c0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_complog
                          where current of c0cursor_9505;
                   endif;
              enddo;
              exsr clsc0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_locnlog
       // check k_locnlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_LOCNLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcll0cursor_9505;
              exsr openl0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from l0cursor_9505
                   into :locnlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:l0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_locnlog
                          where current of l0cursor_9505;
                   endif;
              enddo;
              exsr clsl0cursor_9505;
           endif;
        endif;
       // ---------------------------------------------------------- k_phldlog
       // check k_phldlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PHLDLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclh0cursor_9505;
              exsr openh0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from h0cursor_9505
                   into :phldlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:h0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_phldlog
                          where current of h0cursor_9505;
                   endif;
              enddo;
              exsr clsh0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodlog
       // check k_prodlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclp0cursor_9505;
              exsr openp0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from p0cursor_9505
                   into :prodlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:p0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_prodlog
                           where current of p0cursor_9505;
                   endif;
              enddo;
              exsr clsp0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_supllog
       // check k_supllog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SUPLLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcls0cursor_9505;
              exsr opens0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from s0cursor_9505
                   into :supllog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:s0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_supllog
                          where current of s0cursor_9505;
                   endif;
              enddo;
              exsr clss0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_tabllog
       // check k_tabllog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_TABLLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclt0cursor_9505;
              exsr opent0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from t0cursor_9505
                   into :tabllog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:t0_birth:*days);
                   if  diff_days > tanumber1 AND
                       diff_days > 60;
                       exec sql
                         delete from k_tabllog
                           where current of t0cursor_9505;
                   endif;
              enddo;
              exsr clst0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_userlog
       // check k_userlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_USERLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclu0cursor_9505;
              exsr openu0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from u0cursor_9505
                   into :userlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:u0_birth:*days);
                   if  diff_days > tanumber1 AND
                       diff_days > 60;
                       exec sql
                         delete from k_userlog
                           where current of u0cursor_9505;
                   endif;
              enddo;
              exsr clsu0cursor_9505;
           endif;
        endif;

       // ----------------------------------------------------- end of program
       // ---------------------------------------------------------- k_supdlog
       // check k_supdlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SUPDLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcld0cursor_9505;
              exsr opend0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from d0cursor_9505
                   into :supdlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:d0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_supdlog
                          where current of d0cursor_9505;
                   endif;
              enddo;
              exsr clsd0cursor_9505;
           endif;
        endif;
       // ---------------------------------------------------------- k_svoclog
       // check k_svoclog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SVOCLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclv0cursor_9505;
              exsr openv0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from v0cursor_9505
                   into :svoclog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:v0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_svoclog
                          where current of v0cursor_9505;
                   endif;
              enddo;
              exsr clsv0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_autolog
       // check k_autolog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_AUTOLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcla0cursor_9505;
              exsr opena0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from a0cursor_9505
                   into :autolog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:a0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_autolog
                          where current of a0cursor_9505;
                   endif;
              enddo;
              exsr clsa0cursor_9505;
            endif;
        endif;

       // ---------------------------------------------------------- k_plnklog
       // check k_plnklog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PLNKLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclk0cursor_9505;
              exsr openk0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from k0cursor_9505
                   into :plnklog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:k0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_plnklog
                          where current of k0cursor_9505;
                   endif;
              enddo;
              exsr clsk0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_psrvlog
       // check k_psrvlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PSRVLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclr0cursor_9505;
              exsr openr0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from r0cursor_9505
                   into :psrvlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:r0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_psrvlog
                          where current of r0cursor_9505;
                   endif;
              enddo;
              exsr clsr0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_scdylog
       // check k_scdylog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SCDYLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcly0cursor_9505;
              exsr openy0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from y0cursor_9505
                   into :scdylog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:y0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_scdylog
                          where current of y0cursor_9505;
                   endif;
              enddo;
              exsr clsy0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_scpelog
       // check k_scpelog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_SCPELOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcle0cursor_9505;
              exsr opene0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from e0cursor_9505
                   into :scpelog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:e0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_scpelog
                          where current of e0cursor_9505;
                   endif;
              enddo;
              exsr clse0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_tcenlog
       // check k_tcenlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_TCENLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcln0cursor_9505;
              exsr openn0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from n0cursor_9505
                   into :tcenlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:n0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_tcenlog
                          where current of n0cursor_9505;
                   endif;
              enddo;
              exsr clsn0cursor_9505;
           endif;
        endif;

       // ------------------------------------------------------- k_tspllog
       // check k_tspllog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_TSPLLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclf0cursor_9505;
              exsr openf0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from f0cursor_9505
                   into :tspllog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:f0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_tspllog
                          where current of f0cursor_9505;
                   endif;
              enddo;
              exsr clsf0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_tprdlog
       // check k_tprdlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_TPRDLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclq0cursor_9505;
              exsr openq0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from q0cursor_9505
                   into :tprdlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:q0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_tprdlog
                          where current of q0cursor_9505;
                   endif;
              enddo;
              exsr clsq0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_dperlog
       // check k_dperlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_DPERLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcli0cursor_9505;
              exsr openi0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from i0cursor_9505
                   into :dperlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:i0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_dperlog
                          where current of i0cursor_9505;
                   endif;
              enddo;
              exsr clsi0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_bsrvlog
       // check k_bsrvlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_BSRVLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclg0cursor_9505;
              exsr openg0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from g0cursor_9505
                   into :bsrvlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:g0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_bsrvlog
                          where current of g0cursor_9505;
                   endif;
              enddo;
              exsr clsg0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_gsrvlog
       // check k_gsrvlog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_GSRVLOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclj0cursor_9505;
              exsr openj0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from j0cursor_9505
                   into :gsrvlog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:j0_birth:*days);
                   if  diff_days > tanumber1 AND
                       diff_days > 60;
                       exec sql
                         delete from k_gsrvlog
                           where current of j0cursor_9505;
                   endif;
              enddo;
              exsr clsj0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_notelog
       // check k_notelog file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_NOTELOG '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclm0cursor_9505;
              exsr openm0cursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from m0cursor_9505
                   into :notelog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:m0_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_notelog
                          where current of m0cursor_9505;
                   endif;
              enddo;
              exsr clsm0cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_intdaly
       // check k_intdaly file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INTDALY '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcliecursor_9505;
              exsr openiecursor_9505;
              tr_birth = cmsysdate - %days(tanumber1);
       //     tr_birth = %diff(cmsysdate:tanumber1);
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from iecursor_9505
                   into :intdaly_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   if ie_birth <= tr_birth;
                      exec sql
                        delete from k_intdaly
                          where current of iecursor_9505;
                   endif;
              enddo;
              exsr clsiecursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_prodapp
       // check k_prodapp file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PRODAPP '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclpjcursor_9505;
              exsr openpjcursor_9505;
              tr_birth = cmsysdate - %days(tanumber1);
       //     tr_birth= %diff(cmsysdate:tanumber1:*days);
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from pjcursor_9505
                   into :prodapp_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   if pj_birth <= tr_birth;
                      exec sql
                        delete from k_prodapp
                           where current of pjcursor_9505;
                   endif;
              enddo;
              exsr clspjcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_dlyprod
       // check k_dlyprod file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_DLYPROD '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclr1cursor_9505;
              exsr openr1cursor_9505;
              tr_birth = cmsysdate - %days(tanumber1);
       //     tr_birth = %diff(cmsysdate:tanumber1:*days);
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from r1cursor_9505
                   into :dlyprod_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   if r1_birth <= tr_birth;
                      exec sql
                        delete from k_dlyprod
                           where current of r1cursor_9505;
                   endif;
              enddo;
              exsr clsr1cursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_weekdis
       // check k_weekdis file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_WEEKDIS '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclwdcursor_9505;
              exsr openwdcursor_9505;
              tr_birth = cmsysdate - %days(tanumber1);
       //     tr_birth = %diff(cmsysdate:tanumber1:*days);
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from wdcursor_9505
                   into :weekdis_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   if (wd_birth <= tr_birth);
                      if wd_rectype = 'PW' or
                         wd_rectype = 'SW' or
                         wd_rectype = 'CW' or
                         wd_rectype = 'LW';
                         exec sql
                           delete from k_weekdis
                             where current of wdcursor_9505;
                      endif;
                   endif;
              enddo;
              exsr clswdcursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_intdeal
       // check k_intdeal file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INTDEAL '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclilcursor_9505;
              exsr openilcursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from ilcursor_9505
                   into :intdeal_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:il_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_intdeal
                          where current of ilcursor_9505;
                   endif;
              enddo;
              exsr clsilcursor_9505;
           endif;
        endif;

       // ------------------------------------------------------- k_invprod
       // check k_invprod file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INVPROD '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclincursor_9505;
              exsr openincursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from incursor_9505
                   into :invprod_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:in_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_invprod
                          where current of incursor_9505;
                   endif;
              enddo;
              exsr clsincursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_invsupl
       // check k_invsupl file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_INVSUPL '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclsncursor_9505;
              exsr opensncursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from sncursor_9505
                   into :invsupl_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:sn_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_invsupl
                          where current of sncursor_9505;
                   endif;
              enddo;
              exsr clssncursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_dlysvce
       // check k_dlysvce file

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_DLYSVCE '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcldscursor_9505;
              exsr opendscursor_9505;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from dscursor_9505
                   into :dlysvce_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:ds_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete from k_dlysvce
                          where current of dscursor_9505;
                   endif;
              enddo;
              exsr clsdscursor_9505;
           endif;
        endif;

       // ---------------------------------------------------------- k_perdper
       // check k_perdper file

       exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_PERDPER '
            fetch first row only;

       // table code row found
        if SQLState = SQLStateOk;

       // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclr3cursor_9505;
              exsr openr3cursor_9505;
              tr_birth = cmsysdate - %days(tanumber1);
       //     tr_birth = %diff(cmsysdate:tanumber1:*days);
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from r3cursor_9505
                   into :perdper_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   if (r3_birth <= tr_birth);
                       exec sql
                         delete from k_perdper
                           where current of r3cursor_9505;
                   endif;
              enddo;
              exsr clsr3cursor_9505;
           endif;
        endif;

       // ----------------------------------------------------- end of program
        *inlr = *on;

       begsr dcldmcursor_9505;
         exec sql
           declare dmcursor_9505 Cursor
            for
            select *
            from k_dealsum
            where dm_comp = :comp;
       endsr;

       begsr opendmcursor_9505;
         exec sql
          open dmcursor_9505;
       endsr;

       begsr clsdmcursor_9505;
         exec sql
          close dmcursor_9505;
       endsr;

       begsr dcldgcursor_9505;
         exec sql
           declare dgcursor_9505 Cursor
            for
            select *
            from k_deallog
            where dg_comp = :comp
            for update;
       endsr;

       begsr opendgcursor_9505;
         exec sql
          open dgcursor_9505;
       endsr;

       begsr clsdgcursor_9505;
         exec sql
           close dgcursor_9505;
       endsr;

       begsr dclhacursor_9505;
         exec sql
           declare hacursor_9505 Cursor
            for
            select *
            from k_hstaltr
            where ha_comp = :comp
            for update;
       endsr;

       begsr openhacursor_9505;
         exec sql
          open hacursor_9505;
       endsr;

       begsr clshacursor_9505;
         exec sql
          close hacursor_9505;
       endsr;

       begsr dcliicursor_9505;
         exec sql
           declare iicursor_9505 Cursor
            for
            select *
            from k_intcsti
            where ii_comp = :comp
            for update;
       endsr;

       begsr openiicursor_9505;
         exec sql
          open iicursor_9505;
       endsr;

       begsr clsiicursor_9505;
         exec sql
          close iicursor_9505;
       endsr;

       begsr dclijcursor_9505;
         exec sql
           declare ijcursor_9505 Cursor
            for
            select *
            from k_intcstj
            where ij_comp = :comp
            for update;
       endsr;

       begsr openijcursor_9505;
         exec sql
          open ijcursor_9505;
       endsr;

       begsr clsijcursor_9505;
         exec sql
          close ijcursor_9505;
       endsr;

       begsr dcls8cursor_9505;
         exec sql
           declare s8cursor_9505 Cursor
            for
            select *
            from k_intinpt
            where s8_comp = :comp
            for update;
       endsr;

       begsr opens8cursor_9505;
         exec sql
          open s8cursor_9505;
       endsr;

       begsr clss8cursor_9505;
         exec sql
          close s8cursor_9505;
       endsr;

       begsr dcllgcursor_9505;
         exec sql
           declare lgcursor_9505 Cursor
            for
            select *
            from k_logprod
            where lg_comp = :comp
            for update;
       endsr;

       begsr openlgcursor_9505;
         exec sql
          open lgcursor_9505;
       endsr;

       begsr clslgcursor_9505;
         exec sql
          close lgcursor_9505;
       endsr;

       begsr dcllacursor_9505;
         exec sql
           declare lacursor_9505 Cursor
            for
            select *
            from k_logaltr
            where la_comp = :comp
            for update;
       endsr;

       begsr openlacursor_9505;
         exec sql
          open lacursor_9505;
       endsr;

       begsr clslacursor_9505;
         exec sql
          close lacursor_9505;
       endsr;

       begsr dclgbcursor_9505;
         exec sql
           declare gbcursor_9505 Cursor
            for
            select *
            from k_guestbk
            where gb_comp = :comp or
                  gb_comp = ' '
            for update;
       endsr;

       begsr opengbcursor_9505;
         exec sql
          open gbcursor_9505;
       endsr;

       begsr clsgbcursor_9505;
         exec sql
          close gbcursor_9505;
       endsr;

       begsr dclpocursor_9505;
         exec sql
           declare pocursor_9505 Cursor
            for
            select *
            from k_prodovr
            where po_comp = :comp
            for update;
       endsr;

       begsr openpocursor_9505;
         exec sql
          open pocursor_9505;
       endsr;

       begsr clspocursor_9505;
         exec sql
          close pocursor_9505;
       endsr;

       begsr dclbtcursor_9505;
         exec sql
           declare btcursor_9505 Cursor
            for
            select *
            from k_buyrsot
            where bt_comp = :comp
            for update;
       endsr;

       begsr openbtcursor_9505;
         exec sql
          open btcursor_9505;
       endsr;

       begsr clsbtcursor_9505;
         exec sql
          close btcursor_9505;
       endsr;

       begsr dclcbcursor_9505;
         exec sql
           declare cbcursor_9505 Cursor
            for
            select *
            from k_custbch
            where cb_comp = :comp
            for update;
       endsr;

       begsr opencbcursor_9505;
         exec sql
          open cbcursor_9505;
       endsr;

       begsr clscbcursor_9505;
         exec sql
          close cbcursor_9505;
       endsr;

       begsr dclcpcursor_9505;
         exec sql
           declare cpcursor_9505 Cursor
            for
            select *
            from k_custprd
            where cp_comp = :comp
            for update;
       endsr;

       begsr opencpcursor_9505;
         exec sql
          open cpcursor_9505;
       endsr;

       begsr clscpcursor_9505;
         exec sql
          close cpcursor_9505;
       endsr;

       begsr dclibcursor_9505;
         exec sql
           declare ibcursor_9505 Cursor
            for
            select *
            from k_intordb
            where ib_comp = :comp
            for update;
       endsr;

       begsr openibcursor_9505;
         exec sql
          open ibcursor_9505;
       endsr;

       begsr clsibcursor_9505;
         exec sql
          close ibcursor_9505;
       endsr;

       begsr dclidcursor_9505;
         exec sql
           declare idcursor_9505 Cursor
            for
            select *
            from k_intordd
            where id_comp = :comp
            for update;
       endsr;

       begsr openidcursor_9505;
         exec sql
          open idcursor_9505;
       endsr;

       begsr clsidcursor_9505;
         exec sql
          close idcursor_9505;
       endsr;

       begsr dclntcursor_9505;
         exec sql
           declare ntcursor_9505 Cursor
            for
            select *
            from k_notepad
            where nt_comp = :comp
            for update;
       endsr;

       begsr openntcursor_9505;
         exec sql
          open ntcursor_9505;
       endsr;

       begsr clsntcursor_9505;
         exec sql
          close ntcursor_9505;
       endsr;

       begsr dclpfcursor_9505;
         exec sql
           declare pfcursor_9505 Cursor
            for
            select *
            from k_prodfor
            where pf_comp = :comp
            for update;
       endsr;

       begsr openpfcursor_9505;
         exec sql
          open pfcursor_9505;
       endsr;

       begsr clspfcursor_9505;
         exec sql
          close pfcursor_9505;
       endsr;

       begsr dclphcursor_9505;
         exec sql
           declare phcursor_9505 Cursor
            for
            select *
            from k_prodhis
            where ph_comp = :comp
            for update;
       endsr;

       begsr openphcursor_9505;
         exec sql
          open phcursor_9505;
       endsr;

       begsr clsphcursor_9505;
         exec sql
          close phcursor_9505;
       endsr;

       begsr dclpucursor_9505;
         exec sql
           declare pucursor_9505 Cursor
            for
            select *
            from k_prodhld
            where pu_comp = :comp
            for update;
       endsr;

       begsr openpucursor_9505;
         exec sql
          open pucursor_9505;
       endsr;

       begsr clspucursor_9505;
         exec sql
          close pucursor_9505;
       endsr;

       begsr dclpwcursor_9505;
         exec sql
           declare pwcursor_9505 Cursor
            for
            select *
            from k_prodh52
            where pw_comp = :comp
            for update;
       endsr;

       begsr openpwcursor_9505;
         exec sql
          open pwcursor_9505;
       endsr;

       begsr clspwcursor_9505;
         exec sql
          close pwcursor_9505;
       endsr;

       begsr dclplcursor_9505;
         exec sql
           declare plcursor_9505 Cursor
            for
            select *
            from k_prodltm
            where pl_comp = :comp
            for update;
       endsr;

       begsr openplcursor_9505;
         exec sql
          open plcursor_9505;
       endsr;

       begsr clsplcursor_9505;
         exec sql
          close plcursor_9505;
       endsr;

       begsr dclpbcursor_9505;
         exec sql
           declare pbcursor_9505 Cursor
            for
            select *
            from k_prodseb
            where pb_comp = :comp
            for update;
       endsr;

       begsr openpbcursor_9505;
         exec sql
          open pbcursor_9505;
       endsr;

       begsr clspbcursor_9505;
         exec sql
          close pbcursor_9505;
       endsr;

       begsr dclpdcursor_9505;
         exec sql
           declare pdcursor_9505 Cursor
            for
            select *
            from k_prodsed
            where pd_comp = :comp
            for update;
       endsr;

       begsr openpdcursor_9505;
         exec sql
          open pdcursor_9505;
       endsr;

       begsr clspdcursor_9505;
         exec sql
          close pdcursor_9505;
       endsr;

       begsr dclpscursor_9505;
         exec sql
           declare pscursor_9505 Cursor
            for
            select *
            from k_purchis
            where ps_comp = :comp
            for update;
       endsr;

       begsr openpscursor_9505;
         exec sql
          open pscursor_9505;
       endsr;

       begsr clspscursor_9505;
         exec sql
          close pscursor_9505;
       endsr;

       begsr dclz0cursor_9505;
         exec sql
           declare z0cursor_9505 Cursor
            for
            select *
            from k_pwkdlog
            where z0_comp = :comp
            for update;
       endsr;

       begsr openz0cursor_9505;
         exec sql
          open z0cursor_9505;
       endsr;

       begsr clsz0cursor_9505;
         exec sql
          close z0cursor_9505;
       endsr;

       begsr dclsycursor_9505;
         exec sql
           declare sycursor_9505 Cursor
            for
            select *
            from k_scheddy
            where sy_comp = :comp
            for update;
       endsr;

       begsr opensycursor_9505;
         exec sql
          open sycursor_9505;
       endsr;

       begsr clssycursor_9505;
         exec sql
          close sycursor_9505;
       endsr;

       begsr dclsecursor_9505;
         exec sql
           declare secursor_9505 Cursor
            for
            select *
            from k_schedpe
            where se_comp = :comp
            for update;
       endsr;

       begsr opensecursor_9505;
         exec sql
          open secursor_9505;
       endsr;

       begsr clssecursor_9505;
         exec sql
          close secursor_9505;
       endsr;

       begsr dclslcursor_9505;
         exec sql
           declare slcursor_9505 Cursor
            for
            select *
            from k_suplltm
            where sl_comp = :comp
            for update;
       endsr;

       begsr openslcursor_9505;
         exec sql
          open slcursor_9505;
       endsr;

       begsr clsslcursor_9505;
         exec sql
          close slcursor_9505;
       endsr;

       begsr dclsacursor_9505;
         exec sql
           declare sacursor_9505 Cursor
            for
            select *
            from k_suplpur
            where sa_comp = :comp
            for update;
       endsr;

       begsr opensacursor_9505;
         exec sql
          open sacursor_9505;
       endsr;

       begsr clssacursor_9505;
         exec sql
          close sacursor_9505;
       endsr;

       begsr dclsrcursor_9505;
         exec sql
           declare srcursor_9505 Cursor
            for
            select *
            from k_suplrej
            where sr_comp = :comp
            for update;
       endsr;

       begsr opensrcursor_9505;
         exec sql
          open srcursor_9505;
       endsr;

       begsr clssrcursor_9505;
         exec sql
          close srcursor_9505;
       endsr;

       begsr dclb0cursor_9505;
         exec sql
           declare b0cursor_9505 Cursor
            for
            select *
            from k_buyrlog
            where b0_comp = :comp
            for update;
       endsr;

       begsr openb0cursor_9505;
         exec sql
          open b0cursor_9505;
       endsr;

       begsr clsb0cursor_9505;
         exec sql
          close b0cursor_9505;
       endsr;

       begsr dclc0cursor_9505;
         exec sql
           declare c0cursor_9505 Cursor
            for
            select *
            from k_complog
            where c0_comp = :comp
            for update;
       endsr;

       begsr openc0cursor_9505;
         exec sql
          open c0cursor_9505;
       endsr;

       begsr clsc0cursor_9505;
         exec sql
          close c0cursor_9505;
       endsr;

       begsr dcll0cursor_9505;
         exec sql
           declare l0cursor_9505 Cursor
            for
            select *
            from k_locnlog
            where l0_comp = :comp
            for update;
       endsr;

       begsr openl0cursor_9505;
         exec sql
          open l0cursor_9505;
       endsr;

       begsr clsl0cursor_9505;
         exec sql
          close l0cursor_9505;
       endsr;

       begsr dclh0cursor_9505;
         exec sql
           declare h0cursor_9505 Cursor
            for
            select *
            from k_phldlog
            where h0_comp = :comp
            for update;
       endsr;

       begsr openh0cursor_9505;
         exec sql
          open h0cursor_9505;
       endsr;

       begsr clsh0cursor_9505;
         exec sql
          close h0cursor_9505;
       endsr;

       begsr dclp0cursor_9505;
         exec sql
           declare p0cursor_9505 Cursor
            for
            select *
            from k_prodlog
            where p0_comp = :comp
            for update;
       endsr;

       begsr openp0cursor_9505;
         exec sql
          open p0cursor_9505;
       endsr;

       begsr clsp0cursor_9505;
         exec sql
          close p0cursor_9505;
       endsr;

       begsr dcls0cursor_9505;
         exec sql
           declare s0cursor_9505 Cursor
            for
            select *
            from k_supllog
            where s0_comp = :comp
            for update;
       endsr;

       begsr opens0cursor_9505;
         exec sql
          open s0cursor_9505;
       endsr;

       begsr clss0cursor_9505;
         exec sql
          close s0cursor_9505;
       endsr;

       begsr dclt0cursor_9505;
         exec sql
           declare t0cursor_9505 Cursor
            for
            select *
            from k_tabllog
            where t0_comp = :comp
            for update;
       endsr;

       begsr opent0cursor_9505;
         exec sql
          open t0cursor_9505;
       endsr;

       begsr clst0cursor_9505;
         exec sql
          close t0cursor_9505;
       endsr;

       begsr dclu0cursor_9505;
         exec sql
           declare u0cursor_9505 Cursor
            for
            select *
            from k_userlog
            where u0_comp = :comp
            for update;
       endsr;

       begsr openu0cursor_9505;
         exec sql
          open u0cursor_9505;
       endsr;

       begsr clsu0cursor_9505;
         exec sql
          close u0cursor_9505;
       endsr;

       begsr dcld0cursor_9505;
         exec sql
           declare d0cursor_9505 Cursor
            for
            select *
            from k_supdlog
            where d0_comp = :comp
            for update;
       endsr;

       begsr opend0cursor_9505;
         exec sql
          open d0cursor_9505;
       endsr;

       begsr clsd0cursor_9505;
         exec sql
          close d0cursor_9505;
       endsr;

       begsr dclv0cursor_9505;
         exec sql
           declare v0cursor_9505 Cursor
            for
            select *
            from k_svoclog
            where v0_comp = :comp
            for update;
       endsr;

       begsr openv0cursor_9505;
         exec sql
          open v0cursor_9505;
       endsr;

       begsr clsv0cursor_9505;
         exec sql
          close v0cursor_9505;
       endsr;

       begsr dcla0cursor_9505;
         exec sql
           declare a0cursor_9505 Cursor
            for
            select *
            from k_autolog
            where a0_comp = :comp
            for update;
       endsr;

       begsr opena0cursor_9505;
         exec sql
          open a0cursor_9505;
       endsr;

       begsr clsa0cursor_9505;
         exec sql
          close a0cursor_9505;
       endsr;

       begsr dclk0cursor_9505;
         exec sql
           declare k0cursor_9505 Cursor
            for
            select *
            from k_plnklog
            where k0_comp = :comp
            for update;
       endsr;

       begsr openk0cursor_9505;
         exec sql
          open k0cursor_9505;
       endsr;

       begsr clsk0cursor_9505;
         exec sql
          close k0cursor_9505;
       endsr;

       begsr dclr0cursor_9505;
         exec sql
           declare r0cursor_9505 Cursor
            for
            select *
            from k_psrvlog
            where r0_comp = :comp
            for update;
       endsr;

       begsr openr0cursor_9505;
         exec sql
          open r0cursor_9505;
       endsr;

       begsr clsr0cursor_9505;
         exec sql
          close r0cursor_9505;
       endsr;

       begsr dcly0cursor_9505;
         exec sql
           declare y0cursor_9505 Cursor
            for
            select *
            from k_scdylog
            where y0_comp = :comp
            for update;
       endsr;

       begsr openy0cursor_9505;
         exec sql
          open y0cursor_9505;
       endsr;

       begsr clsy0cursor_9505;
         exec sql
          close y0cursor_9505;
       endsr;

       begsr dcle0cursor_9505;
         exec sql
           declare e0cursor_9505 Cursor
            for
            select *
            from k_scpelog
            where e0_comp = :comp
            for update;
       endsr;

       begsr opene0cursor_9505;
         exec sql
          open e0cursor_9505;
       endsr;

       begsr clse0cursor_9505;
         exec sql
          close e0cursor_9505;
       endsr;

       begsr dcln0cursor_9505;
         exec sql
           declare n0cursor_9505 Cursor
            for
            select *
            from k_tcenlog
            where n0_comp = :comp
            for update;
       endsr;

       begsr openn0cursor_9505;
         exec sql
          open n0cursor_9505;
       endsr;

       begsr clsn0cursor_9505;
         exec sql
          close n0cursor_9505;
       endsr;

       begsr dclf0cursor_9505;
         exec sql
           declare f0cursor_9505 Cursor
            for
            select *
            from k_tspllog
            where f0_comp = :comp
            for update;
       endsr;

       begsr openf0cursor_9505;
         exec sql
          open f0cursor_9505;
       endsr;

       begsr clsf0cursor_9505;
         exec sql
          close f0cursor_9505;
       endsr;

       begsr dclq0cursor_9505;
         exec sql
           declare q0cursor_9505 Cursor
            for
            select *
            from k_tprdlog
            where q0_comp = :comp
            for update;
       endsr;

       begsr openq0cursor_9505;
         exec sql
          open q0cursor_9505;
       endsr;

       begsr clsq0cursor_9505;
         exec sql
          close q0cursor_9505;
       endsr;

       begsr dcli0cursor_9505;
         exec sql
           declare i0cursor_9505 Cursor
            for
            select *
            from k_dperlog
            where i0_comp = :comp
            for update;
       endsr;

       begsr openi0cursor_9505;
         exec sql
          open i0cursor_9505;
       endsr;

       begsr clsi0cursor_9505;
         exec sql
          close i0cursor_9505;
       endsr;

       begsr dclg0cursor_9505;
         exec sql
           declare g0cursor_9505 Cursor
            for
            select *
            from k_bsrvlog
            where g0_comp = :comp
            for update;
       endsr;

       begsr openg0cursor_9505;
         exec sql
          open g0cursor_9505;
       endsr;

       begsr clsg0cursor_9505;
         exec sql
          close g0cursor_9505;
       endsr;

       begsr dclj0cursor_9505;
         exec sql
           declare j0cursor_9505 Cursor
            for
            select *
            from k_gsrvlog
            where j0_comp = :comp
            for update;
       endsr;

       begsr openj0cursor_9505;
         exec sql
          open j0cursor_9505;
       endsr;

       begsr clsj0cursor_9505;
         exec sql
          close j0cursor_9505;
       endsr;

       begsr dclm0cursor_9505;
         exec sql
           declare m0cursor_9505 Cursor
            for
            select *
            from k_notelog
            where m0_comp = :comp
            for update;
       endsr;

       begsr openm0cursor_9505;
         exec sql
          open m0cursor_9505;
       endsr;

       begsr clsm0cursor_9505;
         exec sql
          close m0cursor_9505;
       endsr;

       begsr dcliecursor_9505;
         exec sql
           declare iecursor_9505 Cursor
            for
            select *
            from k_intdaly
            where ie_comp = :comp
            order by ie_comp,
                     ie_birth desc
            for update;
       endsr;

       begsr openiecursor_9505;
         exec sql
          open iecursor_9505;
       endsr;

       begsr clsiecursor_9505;
         exec sql
          close iecursor_9505;
       endsr;

       begsr dclpjcursor_9505;
         exec sql
           declare pjcursor_9505 Cursor
            for
            select *
            from k_prodapp
            where pj_comp = :comp
            order by pj_comp,
                     pj_birth desc
            for update;
       endsr;

       begsr openpjcursor_9505;
         exec sql
          open pjcursor_9505;
       endsr;

       begsr clspjcursor_9505;
         exec sql
          close pjcursor_9505;
       endsr;

       begsr dclr1cursor_9505;
         exec sql
           declare r1cursor_9505 Cursor
            for
            select *
            from k_dlyprod
            where r1_comp = :comp
            for update;
       endsr;

       begsr openr1cursor_9505;
         exec sql
          open r1cursor_9505;
       endsr;

       begsr clsr1cursor_9505;
         exec sql
          close r1cursor_9505;
       endsr;

       begsr dclwdcursor_9505;
         exec sql
           declare wdcursor_9505 Cursor
            for
            select *
            from k_weekdis
            where wd_comp = :comp
            order by wd_comp,
                     wd_birth desc
            for update;
       endsr;

       begsr openwdcursor_9505;
         exec sql
          open wdcursor_9505;
       endsr;

       begsr clswdcursor_9505;
         exec sql
          close wdcursor_9505;
       endsr;

       begsr dclilcursor_9505;
         exec sql
           declare ilcursor_9505 Cursor
            for
            select *
            from k_intdeal
            where il_comp = :comp
            for update;
       endsr;

       begsr openilcursor_9505;
         exec sql
          open ilcursor_9505;
       endsr;

       begsr clsilcursor_9505;
         exec sql
          close ilcursor_9505;
       endsr;

       begsr dclincursor_9505;
         exec sql
           declare incursor_9505 Cursor
            for
            select *
            from k_invprod
            where in_comp = :comp
            for update;
       endsr;

       begsr openincursor_9505;
         exec sql
          open incursor_9505;
       endsr;

       begsr clsincursor_9505;
         exec sql
          close incursor_9505;
       endsr;

       begsr dclsncursor_9505;
         exec sql
           declare sncursor_9505 Cursor
            for
            select *
            from k_invsupl
            where sn_comp = :comp
            for update;
       endsr;

       begsr opensncursor_9505;
         exec sql
          open sncursor_9505;
       endsr;

       begsr clssncursor_9505;
         exec sql
          close sncursor_9505;
       endsr;

       begsr dcldscursor_9505;
         exec sql
           declare dscursor_9505 Cursor
            for
            select *
            from k_dlysvce
            where ds_comp = :comp
            for update;
       endsr;

       begsr opendscursor_9505;
         exec sql
          open dscursor_9505;
       endsr;

       begsr clsdscursor_9505;
         exec sql
          close dscursor_9505;
       endsr;

       begsr dclr3cursor_9505;
         exec sql
           declare r3cursor_9505 Cursor
            for
            select *
            from k_perdper
            where r3_comp = :comp
            for update;
       endsr;

       begsr openr3cursor_9505;
         exec sql
          open r3cursor_9505;
       endsr;

       begsr clsr3cursor_9505;
         exec sql
          close r3cursor_9505;
       endsr;

