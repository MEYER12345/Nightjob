      *****************************************************************
     h copyright('(C) Copyright 1996 - 2014 King III Solutions, Inc.  +
     h Rel 5.1  2014-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2014 by King III Solutions, Inc.
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

     f*k_dealsum if   e             disk                                         table codes
      * deals summary information

     f*k_deallog uf   e             disk                                         table codes
      * log of deal activity

     f*k_hstaltr uf   e             disk                                         table codes
      * history for alternate source transactions

     f*k_intcsti uf   e             disk                                         table codes
      * customer input - header file

     f*k_intcstj uf   e             disk                                         table codes
      * customer input - detail file

     fk_intinpt uf   e             disk                                         table codes
      * customer input - saved


     fk_logprod uf   e             disk                                         table codes
      * log of product transactions

     fk_logaltr uf   e             disk                                         table codes
      * log of alternate source weekly offerings

     fk_prodovr uf   e             disk                                         table codes
      * product overstock transfer requests

     fk_guestbk uf   e             disk                                         table codes
      * guest book

     fk_buyrsot uf   e             disk                                         table codes
      * buy group SOT

     fk_custbch uf   e             disk                                         table codes
      * customer usage batch

     fk_custprd uf   e             disk                                         table codes
      * customer usage products

     fk_intordb uf   e             disk                                         table codes
      * approved order header

     fk_intordd uf   e             disk                                         table codes
      * approved order detail

     fk_notepad uf   e             disk                                         table codes
      * note file

     fk_prodfor uf   e             disk                                         table codes
      * product forcast log

     fk_prodhis uf   e             disk                                         table codes
      * product history 12 & 13

     fk_prodhld uf   e             disk                                         table codes
      * product hold out qtys

     fk_prodh52 uf   e             disk                                         table codes
      * product history 52

     fk_prodltm uf   e             disk                                         table codes
      * product lead time trans

     fk_prodseb uf   e             disk                                         table codes
      * selected product header

     fk_prodsed uf   e             disk                                         table codes
      * selected product detail

     fk_purchis uf   e             disk                                         table codes
      * purchase history

     fk_pwkdlog uf   e             disk                                         table codes
      * product weekly distribution

     fk_scheddy uf   e             disk                                         table codes
      * schedule daily

     fk_schedpe uf   e             disk                                         table codes
      * schedule period end

     fk_suplltm uf   e             disk                                         table codes
      * supplier lead time trans

     fk_suplpur uf   e             disk                                         table codes
      * supplier purchases

     fk_suplrej uf   e             disk                                         table codes
      * supplier rejected alt source

     fk_buyrlog uf   e             disk                                         buy groups
      * buyer log

     fk_complog uf   e             disk                                         buy groups
      * company log

     fk_locnlog uf   e             disk                                         buy groups
      * location log

     fk_phldlog uf   e             disk                                         buy groups
      * hold out log

     fk_prodlog uf   e             disk                                         buy groups
      * product log

     fk_supllog uf   e             disk                                         buy groups
      * supplier log

     fk_tabllog uf   e             disk                                         buy groups
      * table code log

     fk_userlog uf   e             disk                                         buy groups
      * user preference log

     fk_supdlog uf   e             disk                                         buy groups
      * supplier discount brackets log

     fk_svoclog uf   e             disk                                         buy groups
      * supplier variable order cycle log

     fk_autolog uf   e             disk                                         buy groups
      * automated add days log

     fk_plnklog uf   e             disk                                         buy groups
      * product link log

     fk_psrvlog uf   e             disk                                         buy groups
      * product service level exceptions log

     fk_scdylog uf   e             disk                                         buy groups
      * daily processing schedule log

     fk_scpelog uf   e             disk                                         buy groups
      * period end processing schedule log

     fk_tcenlog uf   e             disk                                         buy groups
      * transfer supplier log

     fk_tspllog uf   e             disk                                         buy groups
      * transfer supplier link suppliers log

     fk_tprdlog uf   e             disk                                         buy groups
      * transfer supplier link products log

     fk_dperlog uf   e             disk                                         buy groups
      * permanent deal log

     fk_bsrvlog uf   e             disk                                         buy groups
      * buy group service level log

     fk_gsrvlog uf   e             disk                                         buy groups
      * product group service level log

     fk_notelog uf   e             disk                                         buy groups
      * product group service level log

     fk_intdalyduf   e           k disk                                         table codes
      * approved order detail

     f*_dlyprodduf   e           k disk                                         table codes
      * daily product interface log

     fk_prodappauf   e           k disk                                         table codes
      * product approved/adjusted orders

     fk_weekdiseuf   e           k disk                                         table codes
      * weekly distribution values

     fk_tablcodaif   e           k disk                                         table codes
      * table file

     fk_companyaif   e           k disk                                         buy groups
      * company values

     fk_intdeal uf   e             disk                                         buy groups
      * interface for deal values

     fk_invprod uf   e             disk                                         buy groups
      * metrics for products

     fk_invsupl uf   e             disk                                         buy groups
      * metrics for suppliers

     fk_dlysvce uf   e             disk                                         buy groups
      * daily capture of service fill rate

      * -------------------------------------------------------- work fields
     d comp            s                   like(ta_comp)                        company code
     d diff_days       s              7  0
     d tanumber1       s              5  0
     d userID          s             10
     d test_date       s               d   inz(d'0001-01-01') datfmt(*iso)
     d sysdate         s               d   datfmt(*iso)                         system date (*ISO)
     d tr_birth        s               d   datfmt(*iso)                         system date (*ISO)
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d dealsum_rec   e ds                  ExtName(k_dealsum)
     d deallog_rec   e ds                  ExtName(k_deallog)
     d hstaltr_rec   e ds                  ExtName(k_hstaltr)
     d intcsti_rec   e ds                  ExtName(k_intcsti)
     d intcstj_rec   e ds                  ExtName(k_intcstj)
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

        exec sql
          select cm_sysdate
            into :sysdate
            from k_company
            where cm_comp = :comp;

        exec sql
          select ta_number1
            into :tanumber1
            from k_tablcod
            where ta_comp = :comp and
                  ta_codetyp = 'RET' and
                  ta_codeval = 'K_DEALSUM ';

        if SQLState = SQLStateOk;
        // ---------------------------------------------------------- k_dealsum
        // check k_dealsum file, and get k_dealdet + k_dealalw too

        // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcldscursor;
              exsr opendscursor;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from dscursor
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
              exsr clsdscursor;
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
                  ta_codeval = 'K_DEALLOG ';

        if SQLState = SQLStateOk;
        // ---------------------------------------------------------- k_deallog
        // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcldgcursor;
              exsr opendgcursor;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from dgcursor
                   into :deallog_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:dg_timestp:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete k_deallog
                          where current of dgcursor;
                   endif;
              enddo;
              exsr clsdgcursor;
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
                  ta_codeval = 'K_HSTALTR ';

        if SQLState = SQLStateOk;
        // ---------------------------------------------------------- k_deallog
        // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclhacursor;
              exsr openhacursor;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from hacursor
                   into :hstaltr_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:ha_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete k_hstaltr
                          where current of hacursor;
                   endif;
              enddo;
              exsr clshacursor;
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
                  ta_codeval = 'K_INTCSTI ';

        if SQLState = SQLStateOk;
        // ---------------------------------------------------------- k_deallog
        // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcliicursor;
              exsr openiicursor;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from iicursor
                   into :intcsti_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:ii_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete k_intcsti
                          where current of iicursor;
                   endif;
              enddo;
              exsr clsiicursor;
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
                  ta_codeval = 'K_INTCSTJ ';

        if SQLState = SQLStateOk;
        // ---------------------------------------------------------- k_deallog
        // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dclijcursor;
              exsr openijcursor;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from ijcursor
                   into :intcstj_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:ij_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete k_intcstj
                          where current of ijcursor;
                   endif;
              enddo;
              exsr clsijcursor;
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
                  ta_codeval = 'K_INTINPT ';

        if SQLState = SQLStateOk;
        // ---------------------------------------------------------- k_deallog
        // number of days is valid
           if tanumber1 >= 1 AND tanumber1 <= 99999;
              exsr dcls8cursor;
              exsr opens8cursor;
              Dow SQLState = SQLStateOk;
                 exec sql
                   fetch next
                   from s8cursor
                   into :intinpt_rec;

                   if SQLState = RowNotFound;
                      leave;
                   endif;
                   diff_days = %diff(cmsysdate:s8_birth:*days);
                   if diff_days > tanumber1 AND
                      diff_days > 60;
                      exec sql
                        delete k_intinpt
                          where current of s8cursor;
                   endif;
              enddo;
              exsr clss8cursor;
           endif;
        endif;


      * ---------------------------------------------------------- k_logprod
      * check k_logprod file

     c                   eval      ta_codeval = 'K_LOGPROD '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_logprod                             01

     c                   if        (*in01 = *off) and
     c                             (lg_comp = comp)
     c     cm_sysdate    subdur    lg_timestp    diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_logprod
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_logaltr
      * check k_logaltr file

     c                   eval      ta_codeval = 'K_LOGALTR '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_logaltr                             01

     c                   if        (*in01 = *off) and
     c                             (la_comp = comp)
     c     cm_sysdate    subdur    la_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_logaltr
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_guestbk
      * check k_guestbk file

     c                   eval      ta_codeval = 'K_GUESTBK '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_guestbk                             01

     c                   if        (*in01 = *off) and
     c                             (gb_comp = comp)   OR
     c                             (*in01 = *off) and
     c                             (gb_comp = ' ')
     c     cm_sysdate    subdur    gb_timestr    diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_guestbk
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodovr
      * check k_prodovr file

     c                   eval      ta_codeval = 'K_PRODOVR '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodovr                             01

     c                   if        (*in01 = *off) and
     c                             (po_comp = comp)
     c     cm_sysdate    subdur    po_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodovr
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_buyrsot
      * check k_buyrsot file

     c                   eval      ta_codeval = 'K_BUYRSOT '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_buyrsot                             01

     c                   if        (*in01 = *off) and
     c                             (bt_comp = comp)
     c     cm_sysdate    subdur    bt_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_buyrsot
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_custbch
      * check k_custbch file

     c                   eval      ta_codeval = 'K_CUSTBCH '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_custbch                             01

     c                   if        (*in01 = *off) and
     c                             (cb_comp = comp)
     c     cm_sysdate    subdur    cb_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_custbch
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_custprd
      * check k_custprd file

     c                   eval      ta_codeval = 'K_CUSTPRD '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_custprd                             01

     c                   if        (*in01 = *off) and
     c                             (cp_comp = comp)
     c     cm_sysdate    subdur    cp_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_custprd
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_intordb
      * check k_intordb file

     c                   eval      ta_codeval = 'K_INTORDB '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_intordb                             01

     c                   if        (*in01 = *off) and
     c                             (ib_comp = comp)
     c     cm_sysdate    subdur    ib_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_intordb
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_intordd
      * check k_intordd file

     c                   eval      ta_codeval = 'K_INTORDD '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_intordd                             01

     c                   if        (*in01 = *off) and
     c                             (id_comp = comp)
     c     cm_sysdate    subdur    id_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_intordd
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_notepad
      * check k_notepad file

     c                   eval      ta_codeval = 'K_NOTEPAD '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_notepad                             01

     c                   if        (*in01 = *off) and
     c                             (nt_comp = comp)
     c     cm_sysdate    subdur    nt_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_notepad
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodfor
      * check k_prodfor file

     c                   eval      ta_codeval = 'K_PRODFOR '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodfor                             01

     c                   if        (*in01 = *off) and
     c                             (pf_comp = comp)
     c     cm_sysdate    subdur    pf_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodfor
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodhis
      * check k_prodhis file

     c                   eval      ta_codeval = 'K_PRODHIS '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodhis                             01

     c                   if        (*in01 = *off) and
     c                             (ph_comp = comp)

     c                   move      ph_year       year4             4 0
     c                   movel     year4         date10           10
     c                   move      '-01-01'      date10
     c                   move      date10        test_date

     c     cm_sysdate    subdur    test_date     diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodhis
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodhld
      * check k_prodhld file

     c                   eval      ta_codeval = 'K_PRODHLD '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodhld                             01

     c                   if        (*in01 = *off) and
     c                             (pu_comp = comp)
     c     cm_sysdate    subdur    pu_end        diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodhld
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodh52
      * check k_prodh52 file

     c                   eval      ta_codeval = 'K_PRODH52 '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodh52                             01

     c                   if        (*in01 = *off) and
     c                             (pw_comp = comp)

     c                   move      pw_year       year4
     c                   movel     year4         date10
     c                   move      '-01-01'      date10
     c                   move      date10        test_date

     c     cm_sysdate    subdur    test_date     diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodh52
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodltm
      * check k_prodltm file

     c                   eval      ta_codeval = 'K_PRODLTM '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodltm                             01

     c                   if        (*in01 = *off) and
     c                             (pl_comp = comp)
     c     cm_sysdate    subdur    pl_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodltm
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodseb
      * check k_prodseb file

     c                   eval      ta_codeval = 'K_PRODSEB '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodseb                             01

     c                   if        (*in01 = *off) and
     c                             (pb_comp = comp)
     c     cm_sysdate    subdur    pb_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodseb
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodsed
      * check k_prodsed file

     c                   eval      ta_codeval = 'K_PRODSED '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodsed                             01

     c                   if        (*in01 = *off) and
     c                             (pd_comp = comp)
     c     cm_sysdate    subdur    pd_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodsed
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_purchis
      * check k_purchis file

     c                   eval      ta_codeval = 'K_PURCHIS '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_purchis                             01

     c                   if        (*in01 = *off) and
     c                             (ps_comp = comp)
     c     cm_sysdate    subdur    ps_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_purchis
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_pwkdlog
      * check k_pwkdlog file

     c                   eval      ta_codeval = 'K_PWKDLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_pwkdlog                             01

     c                   if        (*in01 = *off) and
     c                             (z0_comp = comp)
     c     cm_sysdate    subdur    z0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_pwkdlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_scheddy
      * check k_scheddy file

     c                   eval      ta_codeval = 'K_SCHEDDY '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_scheddy                             01

     c                   if        (*in01 = *off) and
     c                             (sy_comp = comp)
     c     cm_sysdate    subdur    sy_sysdate    diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_scheddy
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_schedpe
      * check k_schedpe file

     c                   eval      ta_codeval = 'K_SCHEDPE '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_schedpe                             01

     c                   if        (*in01 = *off) and
     c                             (se_comp = comp)
     c     cm_sysdate    subdur    se_ending     diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_schedpe
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_suplltm
      * check k_suplltm file

     c                   eval      ta_codeval = 'K_SUPLLTM '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_suplltm                             01

     c                   if        (*in01 = *off) and
     c                             (sl_comp = comp)
     c     cm_sysdate    subdur    sl_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_suplltm
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_suplpur
      * check k_suplpur file

     c                   eval      ta_codeval = 'K_SUPLPUR '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_suplpur                             01

     c                   if        (*in01 = *off) and
     c                             (sa_comp = comp)
     c     cm_sysdate    subdur    sa_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_suplpur
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_suplrej
      * check k_suplrej file

     c                   eval      ta_codeval = 'K_SUPLREJ '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_suplrej                             01

     c                   if        (*in01 = *off) and
     c                             (sr_comp = comp)
     c     cm_sysdate    subdur    sr_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_suplrej
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_buyrlog
      * check k_buyrlog file

     c                   eval      ta_codeval = 'K_BUYRLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_buyrlog                             01

     c                   if        (*in01 = *off) and
     c                             (b0_comp = comp)
     c     cm_sysdate    subdur    b0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_buyrlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_complog
      * check k_complog file

     c                   eval      ta_codeval = 'K_COMPLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_complog                             01

     c                   if        (*in01 = *off) and
     c                             (c0_comp = comp)
     c     cm_sysdate    subdur    c0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_complog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_locnlog
      * check k_locnlog file

     c                   eval      ta_codeval = 'K_LOCNLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_locnlog                             01

     c                   if        (*in01 = *off) and
     c                             (l0_comp = comp)
     c     cm_sysdate    subdur    l0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_locnlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_phldlog
      * check k_phldlog file

     c                   eval      ta_codeval = 'K_PHLDLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_phldlog                             01

     c                   if        (*in01 = *off) and
     c                             (h0_comp = comp)
     c     cm_sysdate    subdur    h0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_phldlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodlog
      * check k_prodlog file

     c                   eval      ta_codeval = 'K_PRODLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_prodlog                             01

     c                   if        (*in01 = *off) and
     c                             (p0_comp = comp)
     c     cm_sysdate    subdur    p0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_prodlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_supllog
      * check k_supllog file

     c                   eval      ta_codeval = 'K_SUPLLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_supllog                             01

     c                   if        (*in01 = *off) and
     c                             (s0_comp = comp)
     c     cm_sysdate    subdur    s0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_supllog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_tabllog
      * check k_tabllog file

     c                   eval      ta_codeval = 'K_TABLLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_tabllog                             01

     c                   if        (*in01 = *off) and
     c                             (t0_comp = comp)
     c     cm_sysdate    subdur    t0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_tabllog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_userlog
      * check k_userlog file

     c                   eval      ta_codeval = 'K_USERLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_userlog                             01

     c                   if        (*in01 = *off) and
     c                             (u0_comp = comp)
     c     cm_sysdate    subdur    u0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_userlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ----------------------------------------------------- end of program
      * ---------------------------------------------------------- k_supdlog
      * check k_supdlog file

     c                   eval      ta_codeval = 'K_SUPDLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_supdlog                             01

     c                   if        (*in01 = *off) and
     c                             (d0_comp = comp)
     c     cm_sysdate    subdur    d0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_supdlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_svoclog
      * check k_svoclog file

     c                   eval      ta_codeval = 'K_SVOCLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_svoclog                             01

     c                   if        (*in01 = *off) and
     c                             (v0_comp = comp)
     c     cm_sysdate    subdur    v0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_svoclog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_autolog
      * check k_autolog file

     c                   eval      ta_codeval = 'K_AUTOLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_autolog                             01

     c                   if        (*in01 = *off) and
     c                             (a0_comp = comp)
     c     cm_sysdate    subdur    a0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_autolog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_plnklog
      * check k_plnklog file

     c                   eval      ta_codeval = 'K_PLNKLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_plnklog                             01

     c                   if        (*in01 = *off) and
     c                             (k0_comp = comp)
     c     cm_sysdate    subdur    k0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_plnklog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_psrvlog
      * check k_psrvlog file

     c                   eval      ta_codeval = 'K_PSRVLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_psrvlog                             01

     c                   if        (*in01 = *off) and
     c                             (r0_comp = comp)
     c     cm_sysdate    subdur    r0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_psrvlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_scdylog
      * check k_scdylog file

     c                   eval      ta_codeval = 'K_SCDYLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_scdylog                             01

     c                   if        (*in01 = *off) and
     c                             (y0_comp = comp)
     c     cm_sysdate    subdur    y0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_scdylog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_scpelog
      * check k_scpelog file

     c                   eval      ta_codeval = 'K_SCPELOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_scpelog                             01

     c                   if        (*in01 = *off) and
     c                             (e0_comp = comp)
     c     cm_sysdate    subdur    e0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_scpelog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_tcenlog
      * check k_tcenlog file

     c                   eval      ta_codeval = 'K_TCENLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_tcenlog                             01

     c                   if        (*in01 = *off) and
     c                             (n0_comp = comp)
     c     cm_sysdate    subdur    n0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_tcenlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_tspllog
      * check k_tspllog file

     c                   eval      ta_codeval = 'K_TSPLLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_tspllog                             01

     c                   if        (*in01 = *off) and
     c                             (f0_comp = comp)
     c     cm_sysdate    subdur    f0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_tspllog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_tprdlog
      * check k_tprdlog file

     c                   eval      ta_codeval = 'K_TPRDLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_tprdlog                             01

     c                   if        (*in01 = *off) and
     c                             (q0_comp = comp)
     c     cm_sysdate    subdur    q0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_tprdlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_dperlog
      * check k_dperlog file

     c                   eval      ta_codeval = 'K_DPERLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_dperlog                             01

     c                   if        (*in01 = *off) and
     c                             (i0_comp = comp)
     c     cm_sysdate    subdur    i0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_dperlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_bsrvlog
      * check k_bsrvlog file

     c                   eval      ta_codeval = 'K_BSRVLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_bsrvlog                             01

     c                   if        (*in01 = *off) and
     c                             (g0_comp = comp)
     c     cm_sysdate    subdur    g0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_bsrvlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_gsrvlog
      * check k_gsrvlog file

     c                   eval      ta_codeval = 'K_GSRVLOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_gsrvlog                             01

     c                   if        (*in01 = *off) and
     c                             (j0_comp = comp)
     c     cm_sysdate    subdur    j0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_gsrvlog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_notelog
      * check k_notelog file

     c                   eval      ta_codeval = 'K_NOTELOG '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_notelog                             01

     c                   if        (*in01 = *off) and
     c                             (m0_comp = comp)
     c     cm_sysdate    subdur    m0_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_notelog
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_intdaly
      * check k_intdaly file

     c                   eval      ta_codeval = 'K_INTDALY '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off
     c     *hival        setll     k_intdalyd

     c     cm_sysdate    subdur    ta_number1:*d tr_birth
     c     r1_key        setll     k_intdalyd

     c                   dou       *in01 = *on

     c                   read      rk_intdaly                             01

     c                   if        (*in01 = *off) and
     c                             (ie_comp = comp) and
     c                             (ie_birth <= tr_birth)
     c                   delete    rk_intdaly
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_prodapp
      * check k_prodapp file

     c                   eval      ta_codeval = 'K_PRODAPP '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off
     c     *hival        setll     k_prodappa

     c     cm_sysdate    subdur    ta_number1:*d tr_birth
     c     r1_key        setll     k_prodappa

     c                   dou       *in01 = *on

     c                   read      rk_prodapp                             01

     c                   if        (*in01 = *off) and
     c                             (pj_comp = comp) and
     c                             (pj_birth <= tr_birth)
     c                   delete    rk_prodapp
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_dlyprod
      * check k_dlyprod file

     c                   eval      ta_codeval = 'K_DLYPROD '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999


     c     cm_sysdate    subdur    ta_number1:*d tr_birth

     c/Exec sql
     c+ Delete
     c+  From k_dlyprod
     c+  Where r1_comp   = :comp and
     c+        r1_birth <= :tr_birth
     c/End-exec
     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_weekdis
      * check k_weekdis file

     c                   eval      ta_codeval = 'K_WEEKDIS '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off
     c     *hival        setll     k_weekdise

     c     cm_sysdate    subdur    ta_number1:*d tr_birth
     c     wd_key        setll     k_weekdise

     c                   dou       *in01 = *on

     c                   read      rk_weekdis                             01

     c                   if        (*in01 = *off) and
     c                             (wd_comp = comp) and
     c                             (wd_birth <= tr_birth)
     c                   if        wd_rectype = 'PW' or
     c                             wd_rectype = 'SW' or
     c                             wd_rectype = 'CW' or
     c                             wd_rectype = 'LW'
     c                   delete    rk_weekdis
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_intdeal
      * check k_intdeal file

     c                   eval      ta_codeval = 'K_INTDEAL '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_intdeal                             01

     c                   if        (*in01 = *off) and
     c                             (il_comp = comp)
     c     cm_sysdate    subdur    il_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_intdeal
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_invprod
      * check k_invprod file

     c                   eval      ta_codeval = 'K_INVPROD '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_invprod                             01

     c                   if        (*in01 = *off) and
     c                             (in_comp = comp)
     c     cm_sysdate    subdur    in_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_invprod
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_invsupl
      * check k_invsupl file

     c                   eval      ta_codeval = 'K_INVSUPL '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_invsupl                             01

     c                   if        (*in01 = *off) and
     c                             (sn_comp = comp)
     c     cm_sysdate    subdur    sn_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_invsupl
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ---------------------------------------------------------- k_dlysvce
      * check k_dlysvce file

     c                   eval      ta_codeval = 'K_DLYSVCE '

     c     ta_key        chain     rk_tablcod                         99

      *     table code record found
     c                   if        *in99 = *off

      *     number of days is valid
     c                   if        ta_number1 >= 1 AND ta_number1 <= 99999

     c                   eval      *in01 = *off

     c                   dou       *in01 = *on

     c                   read      rk_dlysvce                             01

     c                   if        (*in01 = *off) and
     c                             (ds_comp = comp)
     c     cm_sysdate    subdur    ds_birth      diff_days:*d
     c                   if        diff_days > ta_number1 AND
     c                             diff_days > 60
     c                   delete    rk_dlysvce
     c                   endif
     c                   endif

     c                   enddo

     c                   endif

     c                   endif

      * ----------------------------------------------------- end of program
     c                   eval      *inlr = *on
       begsr dcldscursor;
         exec sql
           declare dscursor Cursor
            for
            select *
            from k_dealsum
            where dm_comp = :comp;
        endsr;

        begsr opendscursor;
          exec sql
           open dscursor;
        endsr;

        begsr clsdscursor;
          exec sql
           close dscursor;
        endsr;

       begsr dcldgcursor;
         exec sql
           declare dgcursor Cursor
            for
            select *
            from k_deallog
            where dg_comp = :comp
            for update;
        endsr;

        begsr opendgcursor;
          exec sql
           open dgcursor;
        endsr;

        begsr clsdgcursor;
        i  exec sql

           close dgcursor;
        endsr;

       begsr dclhacursor;
         exec sql
           declare hacursor Cursor
            for
            select *
            from k_hstaltr
            where ha_comp = :comp
            for update;
        endsr;

        begsr openhacursor;
          exec sql
           open hacursor;
        endsr;

        begsr clshacursor;
          exec sql
           close hacursor;
        endsr;

       begsr dcliicursor;
         exec sql
           declare iicursor Cursor
            for
            select *
            from k_intcsti
            where ii_comp = :comp
            for update;
        endsr;

        begsr openiicursor;
          exec sql
           open iicursor;
        endsr;

        begsr clsiicursor;
          exec sql
           close iicursor;
        endsr;

       begsr dclijcursor;
         exec sql
           declare ijcursor Cursor
            for
            select *
            from k_intcstj
            where ij_comp = :comp
            for update;
       endsr;

       begsr openijcursor;
         exec sql
          open ijcursor;
       endsr;

       begsr clsijcursor;
         exec sql
          close ijcursor;
       endsr;

       begsr dcls8cursor;
         exec sql
           declare s8cursor Cursor
            for
            select *
            from k_intinpt
            where s8_comp = :comp
            for update;
       endsr;

        begsr opens8cursor;
          exec sql
           open s8cursor;
        endsr;

        begsr clss8cursor;
          exec sql
           close s8cursor;
        endsr;

