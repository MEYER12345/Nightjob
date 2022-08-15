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
      **   Name: K3S_3010
      **   Type: ILE RPG Program
      **   Desc: Product copy history
      **
      *****************************************************************
      **
      **  This program is used to copy product history
      **
      *****************************************************************

     fk3s_3010fmcf   e             workstn infds(infds)
     f                                     usropn

     fk_productauf   e           k disk
      * product file

     fk_productgif   e           k disk    rename(rk_product:r1_product)
      * product file

     fk_supliercif   e           k disk
      * suppliers by location, supplier, sub supplier

     fk_prodhisbuf a e           k disk
      * products histopry

     fk_prodh52buf a e           k disk
      * products histopry

     fk_tablcodaif   e           k disk
      * table file

     fk_prodsoqbuf   e           k disk
      * selected products batches

     fk_suplsoqauf   e           k disk
      * selected products batches

     fk_locatnsaif   e           k disk
      * locations by location

      * ------------------------------------ File information data structure
     d/copy k3s_c010

      * ------------------------------------------- Function key definitions
     d/copy k3s_c020

      * ---------------------------------------------------- Local Data Area
     d/copy k3s_c030

      * -------------------------------------- Program Status Data Structure
     d/copy k3s_c040

      * ---------------------------------- Message logic for screen programs
     d/copy k3s_c050

      * ---------------------------------- Display headings for history
     d/copy k3s_c140

      * --------------------------------- common D specs
     d/copy k3s_c270

      * --------------------------------- L and R adjust for prod ID
     d/copy k3s_c280

      * -------------------------------------------------- parameters passed
     d accdem          s              7  0 dim(52)
     d updtype         s                   like(pr_comp)
     d addrep          s                   like(pr_comp)
     d usrstat         s                   like(pr_usrstat)
     d frezend         s                   like(zz_frezend)
     d setfore         s                   like(zz_setfore)
     d multply         s                   like(zz_multply)
     d tlocn           s                   like(pr_locn)
     d tsupl           s                   like(pr_supl)
     d tsuplsub        s                   like(pr_suplsub)
     d xx_tosupl       s                   like(pr_supl)
     d xx_tosub        s                   like(pr_suplsub)
     d xx_frsupl       s                   like(pr_supl)
     d xx_frsub        s                   like(pr_suplsub)
     d xx_forcyr       s                   like(pr_forcyr)
     d xx_forcper      s                   like(pr_forcper)
     d zz_forcyr       s                   like(pr_forcyr)
     d zz_forcper      s                   like(pr_forcper)
     d supla           s                   like(pr_supl)
     d suplsuba        s                   like(pr_suplsub)
     d tprod           s                   like(pr_prod)
     d xx_comp         s                   like(pr_comp)
     d savebirth       s                   like(pr_birth)
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
     d sz_usrstat      s                   like(pr_usrstat)
     d sz_frezend      s                   like(zz_frezend)
     d sz_setfore      s                   like(pr_usrstat)
     d sz_forcast      s                   like(pr_forcast)
     d sz_fordevp      s                   like(pr_fordevp)
     d zz_fordevp      s                   like(pr_fordevp)
     d zz_frforci      s                   like(pr_forcint)
     d dl_forcint      s                   like(pr_forcint)
     d zz_frbuyr       s                   like(pr_buyr)
     d zz_frso#        s                   like(so_soqseq#)
     d zz_fstslpr      s                   like(pr_fstslpr)
     d zz_fstslyr      s                   like(pr_fstslyr)
     d sz_frsupl       s                   like(pr_supl)
     d sz_frsub        s                   like(pr_suplsub)
     d sz_frprod       s                   like(pr_prod)
     d sz_tolocn       s                   like(pr_locn)
     d zz_toforci      s                   like(pr_forcint)
     d zz_tobuyr       s                   like(pr_buyr)
     d zz_toso#        s                   like(so_soqseq#)
     d zz_toso#1       s                   like(so_soqseq#)
     d sz_tosupl       s                   like(pr_supl)
     d sz_tosub        s                   like(pr_suplsub)
     d sz_toprod       s                   like(pr_prod)

     d diff19          s              1
     d addmsg          s              1
     d add             s              1
     d replace         s              1
     d domany          s              1
     d ck_userupd      s              1  0
     d @_normal1       s              1a
     d save_rrn        s              5  0
     d pf_chgd1        s              7
     d pf_chgd2        s             13
     d day_differ      s              5  0
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

      * ------------------------------- Named indicators for file operations
     d tablpsuNOT      c                   18
     d tablphtNOT      c                   14
     d suplsoqNOT      c                   16
     d supliefNOT      c                   19
     d suplietNOT      c                   20
     d prodsoqNOT      c                   17
     d locatnfNOT      c                   15
     d locatntNOT      c                   22
     d producfNOT      c                   27
     d productNOT      c                   28
      * -------------------------------------------------- Parameters passed

      * parameters passed to program
     c     *entry        plist
     c                   parm                    comp
     c                   parm                    buyr
     c                   parm                    locn
     c                   parm                    supl
     c                   parm                    suplsub
     c                   parm                    prod
     c                   parm                    @updated
     c                   parm                    @returned
     c                   parm                    mode
     c                   parm                    soqseq#
     c                   parm                    prodseq
     c                   parm                    program
     c                   parm                    program2
     c                   parm                    checktype
     c                   parm                    supla
     c                   parm                    suplsuba
     c                   parm                    rollupdown
     c                   parm                    mode1
     c                   parm                    histype
     c                   parm                    updtype
     c                   parm                    tlocn
     c                   parm                    tsupl
     c                   parm                    tsuplsub
     c                   parm                    tprod
     c                   parm                    addrep
     c                   parm                    usrstat
     c                   parm                    frezend
     c                   parm                    setfore
     c                   parm                    multply

      * ---------------------------------------------------------- Key Lists

      * key list for table file
     c     ta_key        klist
     c                   kfld                    ta_comp
     c                   kfld                    ta_codetyp
     c                   kfld                    ta_codeval

      * key list for locations file
     c     lc_key        klist
     c                   kfld                    comp
     c                   kfld                    locn

     c     lc_keyx       klist
     c                   kfld                    xx_comp

     c     lc_keyy       klist
     c                   kfld                    comp

     c     lc_frkey      klist
     c                   kfld                    comp
     c                   kfld                    zz_frlocn

     c     lc_tokey      klist
     c                   kfld                    comp
     c                   kfld                    zz_tolocn

     c     so_key        klist
     c                   kfld                    comp
     c                   kfld                    buyr
     c                   kfld                    locn
     c                   kfld                    supl
     c                   kfld                    suplsub
     c                   kfld                    soqseq#

     c     so_frkey      klist
     c                   kfld                    comp
     c                   kfld                    zz_frbuyr
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub
     c                   kfld                    zz_frso#

     c     so_tokey      klist
     c                   kfld                    comp
     c                   kfld                    zz_tobuyr
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub
     c                   kfld                    zz_toso#

     c     so_tokey1     klist
     c                   kfld                    comp
     c                   kfld                    zz_tobuyr
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub
     c                   kfld                    zz_toso#1

     c     pq_key        klist
     c                   kfld                    comp
     c                   kfld                    buyr
     c                   kfld                    locn
     c                   kfld                    supl
     c                   kfld                    suplsub
     c                   kfld                    soqseq#
     c                   kfld                    prod

     c     pq_frkey      klist
     c                   kfld                    comp
     c                   kfld                    zz_frbuyr
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub
     c                   kfld                    zz_frso#
     c                   kfld                    zz_frprod

     c     pq_tokey      klist
     c                   kfld                    comp
     c                   kfld                    zz_tobuyr
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub
     c                   kfld                    zz_toso#
     c                   kfld                    zz_toprod

     c     pq_tokey1     klist
     c                   kfld                    comp
     c                   kfld                    zz_tobuyr
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub
     c                   kfld                    zz_toso#1
     c                   kfld                    zz_toprod

      * key list for table code file
     c     pr_key        klist
     c                   kfld                    comp
     c                   kfld                    locn
     c                   kfld                    supl
     c                   kfld                    suplsub
     c                   kfld                    prod

     c     pr_frkey      klist
     c                   kfld                    comp
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub
     c                   kfld                    zz_frprod

     c     pr_tokey      klist
     c                   kfld                    comp
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub
     c                   kfld                    zz_toprod

     c     pr_frke       klist
     c                   kfld                    comp
     c                   kfld                    zz_frprod

     c     pr_toke       klist
     c                   kfld                    comp
     c                   kfld                    zz_toprod

     c     pr_frnew      klist
     c                   kfld                    comp
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub
     c                   kfld                    zz_frprod

     c     pr_tonew      klist
     c                   kfld                    comp
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub
     c                   kfld                    zz_toprod

      * key list for supplier file by supplier
     c     sp_key        klist
     c                   kfld                    comp
     c                   kfld                    locn
     c                   kfld                    supl
     c                   kfld                    suplsub

     c     sp_frkey      klist
     c                   kfld                    comp
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub

     c     sp_tokey      klist
     c                   kfld                    comp
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub

     c     sp_frke       klist
     c                   kfld                    comp
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub

     c     sp_toke       klist
     c                   kfld                    comp
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub

      * key list for product history file
     c     ph_frkey      klist
     c                   kfld                    comp
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub
     c                   kfld                    zz_frprod
     c                   kfld                    zz_frforci
     c                   kfld                    zz_histype

     c     ph_frkey1     klist
     c                   kfld                    comp
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub
     c                   kfld                    zz_frprod
     c                   kfld                    zz_frforci
     c                   kfld                    zz_histype
     c                   kfld                    xx_year

     c     ph_tokey      klist
     c                   kfld                    comp
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub
     c                   kfld                    zz_toprod
     c                   kfld                    zz_toforci
     c                   kfld                    zz_histype
     c                   kfld                    xx_year

     c     ph_frnew      klist
     c                   kfld                    comp
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub
     c                   kfld                    zz_frprod
     c                   kfld                    zz_frforci
     c                   kfld                    zz_histype

     c     ph_tonew      klist
     c                   kfld                    ph_comp
     c                   kfld                    ph_locn
     c                   kfld                    ph_supl
     c                   kfld                    ph_suplsub
     c                   kfld                    ph_prod
     c                   kfld                    ph_forcint
     c                   kfld                    ph_histype
     c                   kfld                    ph_year

     c     pw_tonew      klist
     c                   kfld                    pw_comp
     c                   kfld                    pw_locn
     c                   kfld                    pw_supl
     c                   kfld                    pw_suplsub
     c                   kfld                    pw_prod
     c                   kfld                    pw_forcint
     c                   kfld                    pw_histype
     c                   kfld                    pw_year

     c     pw_frnew      klist
     c                   kfld                    comp
     c                   kfld                    zz_frlocn
     c                   kfld                    zz_frsupl
     c                   kfld                    zz_frsub
     c                   kfld                    zz_frprod

     c     ph_toold      klist
     c                   kfld                    comp
     c                   kfld                    zz_tolocn
     c                   kfld                    zz_tosupl
     c                   kfld                    zz_tosub
     c                   kfld                    zz_toprod
     c                   kfld                    zz_toforci
     c                   kfld                    zz_histype


      * ------------------------------------------------------- Once Routine
      * once routine
     c                   exsr      $_once
     c                   eval      key_press = enter_key
     c                   eval      xx_comp = *hival
     c                   eval      diff19 = *off

     c                   if        updtype = 'O'
     c                   move      *blanks       pf_chgdesc
     c                   movel     'User made ch'pf_chgdesc
     c                   move      'ange    '    pf_chgdesc
     c                   endif
     c                   if        updtype = 'B'
     c                   move      *blanks       pf_chgdesc
     c                   movel     'K3S_3011 mad'pf_chgdesc
     c                   move      'e change'    pf_chgdesc
     c                   endif


      * get user update authority flag
      * call module to obtain user update authority flag
     c                   if        updtype = 'O'
     c                   call      'K3S_9050'
     c                   parm                    zz_program
     c                   parm      0             @usr_updat
     c                   eval      *in94 =      (@usr_updat = 0)
     c                   eval      *in97 = *off
     c                   if        (@usr_updat = 0) or
     c                             (mode1 = 'V')
     c                   eval      *in97 = *on
     c                   endif
     c                   endif

      * prime F8=User exit text
     c                   exsr      $_usr_exit

      * get screen title
     c                   eval      @recd_frmt = '1R'
     c                   exsr      $_get_titl

      * get time formatted
     c                   if        updtype = 'O'
     c                   exsr      $_get_time
     c                   endif


      * -------------------------------- display lead time transactions loop

      * continue until user decides to get out
     c                   dow       (key_press <> f03_key) and
     c                             (finished = *off)

      * controlled loop for comand key display
     c                   dou       (key_press <> f24_key) and
     c                             (re_display = *off)

      * if errors exist, and user set for alarm, sound alarm
     c                   if        (errors = *on) and
     c                             (lda_alarm = 1)
     c                   eval      *in98 = *on
     c                   exsr      $_pass_msg
     c                   else
     c                   eval      *in98 = *off
     c                   exsr      $_pass_msg
     c                   endif

     c                   if        errors = *off
     c                   eval      *in61 = *on
     c                   endif
      * ------------------------------------- lead time transactions screens
     c                   if        (*in(tablpsuNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (*in(locatnfNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (*in(locatntNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (*in(tablphtNOT) = *on) and
     c                             (lda_window = 1)
     c                   if        updtype = 'O'
     c                   exsr      $_cur_sel
     c                   endif
     c                   if        updtype = 'B'
     c                   eval      key_press = f03_key
     c                   endif
     c                   eval      *in(tablpsuNOT) = *off
     c                   eval      *in(tablphtNOT) = *off
     c                   eval      *in(locatnfNOT) = *off
     c                   eval      *in(locatntNOT) = *off
     c                   else
     c                   if        updtype = 'O'
     c                   write     k3_ctl_msg
     c                   exfmt     k3_3010_1r
      * Right adjust
     c                   if        rightadj = *on
     c                   call      'K3S_M170'
     c                   parm      zz_frprod     prodin                         supplier
     c                   parm                    prodlen                        sub supplier
     c                   parm                    prodout                        sub supplier
     c                   eval      zz_frprod  = prodout
     c                   call      'K3S_M170'
     c                   parm      zz_toprod     prodin                         supplier
     c                   parm                    prodlen                        sub supplier
     c                   parm                    prodout                        sub supplier
     c                   eval      zz_toprod  = prodout
     c                   endif
      * Right adjust
     c                   endif
     c                   endif
      * user has pressed F15=add history
     c                   if        key_press = f15_key or
     c                             addrep = 'A'
     c                   eval      add = *on
     c                   eval      replace = *off
     c                   endif

      * user has pressed F19=replace history
     c                   if        key_press = f19_key or
     c                             addrep = 'R'
     c                   eval      add = *off
     c                   eval      replace = *on
     c                   endif
      * user has pressed F03=Exit to exit without update
     c                   if        key_press = f03_key
     c                   eval      finished = *on
     c                   eval      @updated  = 0
     c                   eval      @returned = 3
     c                   endif

      * user has pressed F12=Previous to return without update
     c                   if        key_press = f12_key
     c                   eval      finished = *on
     c                   eval      @updated  = 0
     c                   eval      @returned = 12
     c                   endif


      * Function key 4 (prompt) was pressed.
     c                   if        key_press = f04_key
      * Check posistion of cursor on the screen.
     c                   exsr      $_cur_pos
      * Call appropriate selection program if applicable.
     c                   exsr      $_cur_sel
     c                   endif
      * subfile message control
     c                   eval      *in99 = *off

      * sound alarm indicator
     c                   eval      *in98 = *off

      * set off error
     c                   eval      errors = *off


      * re-display after valid command key list
     c                   eval      re_display = *off

      * display valid comand keys
     c                   if        key_press = f24_key
      * get time formatted
     c                   exsr      $_get_time
     c                   exfmt     K3_3010_1f
      * user has pressed F03=Exit to exit without update
     c                   if        key_press = f03_key
     c                   eval      finished = *on
     c                   eval      @updated  = 0
     c                   eval      @returned = 3
     c                   endif

      * user has pressed F12=Previous to return without update
     c                   if        key_press = f12_key
     c                   eval      finished = *on
     c                   eval      @updated  = 0
     c                   eval      @returned = 12
     c                   endif


      ******************************************
      * maintain user preferences
      *
     c                   if        key_press = f24_key
     c                   call      'K3S_9040CL'
     c                   parm                    zz_program
     c                   parm      *zeros        @returned
     c                   if        @returned = 3
     c                   eval      finished = *on
     c                   eval      key_press = f03_key
     c                   endif

      * retrieve data area *lda
     c                   exsr      $_get_lda
     c                   endif

      ******************************************


      * re-display if user presses enter key, else user selects function key
     c                   if        key_press = enter_key
     c                   eval      re_display = *on
     c                   endif
     c                   endif

     c                   enddo


     c                   if        (zz_frlocn  <> sz_frlocn)   or
     c                             (zz_frsupl  <> sz_frsupl)   or
     c                             (zz_frsub   <> sz_frsub)   or
     c                             (zz_frprod  <> sz_frprod)   or
     c                             (zz_tolocn  <> sz_tolocn)   or
     c                             (zz_tosupl  <> sz_tosupl)   or
     c                             (zz_tosub   <> sz_tosub)   or
     c                             (zz_toprod  <> sz_toprod)   or
     c                             (zz_histype <> sz_histype) or
     c                             (zz_usrstat <> sz_usrstat) or
     c                             (zz_frezend <> sz_frezend) or
     c                             (zz_setfore <> sz_setfore) or
     c                             (key_press = f15_key) or
     c                             (key_press = f19_key) or
     c                             (addrep = 'A') or
     c                             (addrep = 'R')

      *     edit location entry
     c                   exsr      $_edt_locn
      *     edit supplier entry
     c                   if        errors = *off
     c                   exsr      $_edt_supl
     c                   endif
      *     edit product entry
     c                   if        errors = *off
     c                   exsr      $_edt_prod
     c                   endif
      *     edit history type entry
     c                   if        errors = *off
     c                   exsr      $_edt_type
     c                   endif
      *     edit user status
     c                   if        errors = *off
     c                   exsr      $_edt_sta1
     c                   endif
      *     edit date
     c                   clear                   iso_end
     c                   if        errors = *off and zz_frezend > *blanks or
     c                             errors = *off and zz_usrstat = 'F'
     c                   if        updtype = 'O'
     c                   exsr      $_edt_endz
     c                   endif
     c                   if        updtype = 'B' and
     c                             frezend <> *blanks
     c                   move      frezend       iso_end
     c                   endif
     c                   endif
     c                   eval      sz_frlocn = zz_frlocn
     c                   eval      sz_frsupl = zz_frsupl
     c                   eval      sz_frsub  = zz_frsub
     c                   eval      sz_frprod = zz_frprod
     c                   eval      sz_tolocn = zz_tolocn
     c                   eval      sz_tosupl = zz_tosupl
     c                   eval      sz_tosub  = zz_tosub
     c                   eval      sz_toprod = zz_toprod
     c                   eval      sz_histype = zz_histype
     c                   eval      sz_usrstat = zz_usrstat
     c                   eval      sz_frezend = zz_frezend
     c                   eval      sz_setfore = zz_setfore
     c                   endif

      * f01 HELP exit
     c/copy K3S_C192

      * f08 user exit
     c                   if        key_press = f08_key
     c                   eval      @8_locn = locn
     c                   eval      @8_buyr = buyr
     c                   eval      @8_supl = supl
     c                   eval      @8_suplsub = suplsub
     c                   eval      @8_prod = prod
     c                   exsr      $_f8_exit
     c                   endif

      * end of main loop
     c                   if        updtype = 'O' and
     c                             diff19 = *on
     c                   dou       key_press = f12_key or
     c                             key_press = f19_key
     c                   eval      errors    = *on
     c                   write     k3_ctl_msg
     c                   write     k3_3010_1r
     c                   exfmt     k3_3010_wn
     c                   if        key_press = f12_key
     c                   eval      finished = *on
     c                   eval      @updated  = 0
     c                   eval      @returned = 12
     c                   eval      diff19    = *off
     c                   endif
     c                   if        key_press = f19_key
     c                   eval      errors = *off
     c                   eval      diff19    = *on
     c                   endif
     c                   enddo
     c                   endif
      * user has pressed F12=Previous to return without update
     c                   if        (key_press = f03_key) or
     c                             (key_press = f12_key)
     c                   eval      finished = *on
     c                   eval      @updated = 1
     c                   else
     c                   if        (add = *on) or
     c                             (replace = *on)
     c                   if        errors = *off
     c                   exsr      $_hist_chk
     c                   endif
     c                   endif
     c                   endif
     c/copy k3s_c250
     c                   if        updtype = 'B'
     c                   eval      key_press = f03_key
     c                   eval      finished = *on
     c                   eval      @updated = 1
     c                   endif
     c                   enddo

      * --------------------------------------------------- End of Main Loop

      * finished, set on LR
     c                   if        updtype = 'O'
     c                   close     k3s_3010fm                           62
     c                   endif
     c                   eval      *inlr = *on

      * ***************************************************** End of program

      * /////////////////////////////////////////////////////// Once routine

     c     $_once        begsr
     c                   if        updtype = 'O'
     c                   open      k3s_3010fm                           62
     c                   endif
     c                   eval      addmsg = *off
     c                   if        (checktype = *blanks)
     c                   eval      zz_modedsp = %subst('  -' : 1) + 'all- '
     c                   else
     c                   eval      zz_modedsp = %subst('  -' : 1) +
     c                             checktype + '-  '
     c                   endif
     c                   if        mode = '-select-'
     c                   eval      zz_modedsp = %subst(mode : 1)
     c                   endif

      * prime program message queue
     c                   eval      msgpgq = '*'

     c                   eval      zz_modedsp = mode
      * retrieve data area *lda, and format information
     c                   if        updtype = 'O'
     c                   exsr      $_get_lda
     c                   endif

     c     lc_key        chain(n)  rk_locatns                         15
     c                   if        updtype = 'B' and
     c                             locn = *blanks
     c                   eval      *in15 = *off
     c                   endif
      * prime program id
     c                   eval      zz_program = psds_progm

      * prime program message queue
     c                   eval      msgpgq = '*'

      * prime program id
     c                   eval      zz_program = psds_progm

      * prime company code
     c                   eval      zz_compcod = lda_compcd

      * prime user id
     c                   eval      zz_user = psds_user

      * prime table code id

      * set finished indicator to off
     c                   eval      finished = *off

      * set error off
     c                   eval      errors = *off

      * position cursor to location
     c                   eval      *in54 = *on
     c                   eval      *in61 = *on
     c                   eval      *in25 = *off

      * get product
     c     pr_key        chain(n)  rk_product                         55
     c     pq_key        chain(n)  rk_prodsoq                         55
     c                   if        updtype = 'O'
     c                   eval      zz_frlocn  = locn
     c                   eval      zz_frbuyr  = pr_buyr
     c                   eval      zz_tobuyr  = pr_buyr
     c                   eval      zz_frforci = pr_forcint
     c                   eval      zz_toso#1  = 001
     c                   eval      zz_toso#   = pq_soqseq#
     c                   eval      zz_frso#   = pq_soqseq#
     c                   eval      zz_toforci = pr_forcint
     c                   eval      zz_frsupl  = supl
     c                   eval      zz_frsub   = suplsub
     c                   eval      zz_frprod  = prod
     c                   eval      zz_frdesc1 = pr_desc1
     c                   eval      zz_frdesc2 = pr_desc2
     c                   eval      zz_tolocn  = locn
     c                   eval      zz_tosupl  = supl
     c                   eval      zz_tosub   = suplsub
     c                   eval      zz_toprod  = prod
     c                   eval      zz_todesc1 = pr_desc1
     c                   eval      zz_todesc2 = pr_desc2
     c                   eval      zl_frlocn  = locn
     c                   eval      zl_frsupl  = supl
     c                   eval      zl_frsub   = suplsub
     c                   eval      zl_frprod  = prod
     c                   eval      zl_frdesc1 = pr_desc1
     c                   eval      zl_tolocn  = locn
     c                   eval      zl_tosupl  = supl
     c                   eval      zl_tosub   = suplsub
     c                   eval      zl_toprod  = prod
     c                   eval      zl_todesc1 = pr_desc1
     c                   eval      zz_multply = 1.0000
     c                   eval      zl_multply = 1.0000
     c                   eval      zz_histype = histype
     c                   eval      zl_histype = histype
     c                   eval      sz_histype = histype
     c                   eval      zz_setfore = '0'
     c                   eval      sz_setfore = '0'
     c                   eval      *in80 = *off
     c                   exsr      $_hist_ck1
     c                   eval      @returned = *zeros
     c                   eval      key_press = enter_key
     c                   endif
     c                   if        updtype = 'B'
     c                   eval      zz_frlocn  = locn
     c                   eval      zz_frbuyr  = pr_buyr
     c                   eval      zz_tobuyr  = pr_buyr
     c                   eval      zz_frforci = pr_forcint
     c                   eval      zz_toso#1  = 001
     c                   eval      zz_toso#   = pq_soqseq#
     c                   eval      zz_frso#   = pq_soqseq#
     c                   eval      zz_toforci = pr_forcint
     c                   eval      zz_frsupl  = supl
     c                   eval      zz_frsub   = suplsub
     c                   eval      zz_frprod  = prod
     c                   eval      zz_tolocn  = tlocn
     c                   eval      zz_tosupl  = tsupl
     c                   eval      zz_tosub   = tsuplsub
     c                   eval      zz_toprod  = tprod
     c                   eval      zl_frlocn  = locn
     c                   eval      zl_frsupl  = supl
     c                   eval      zl_frsub   = suplsub
     c                   eval      zl_frprod  = prod
     c                   eval      zl_tolocn  = tlocn
     c                   eval      zl_tosupl  = tsupl
     c                   eval      zl_tosub   = tsuplsub
     c                   eval      zl_toprod  = tprod
     c                   if        multply = 0
     c                   eval      zz_multply = 1.0000
     c                   eval      zl_multply = 1.0000
     c                   else
     c                   eval      zz_multply = multply
     c                   eval      zl_multply = multply
     c                   endif
     c                   if        setfore = '1'
     c                   eval      zz_setfore = setfore
     c                   else
     c                   eval      zz_setfore = '0'
     c                   endif
     c                   if        frezend <> *blanks and
     c                             frezend <> '0001-01-01'
     c                   eval      zz_frezend = frezend
     c                   else
     c                   eval      zz_frezend = *blanks
     c                   endif
     c                   if        usrstat <> *blanks
     c                   eval      zz_usrstat = usrstat
     c                   else
     c                   eval      zz_usrstat = *blanks
     c                   endif
     c                   eval      zz_histype = histype
     c                   eval      zl_histype = histype
     c                   eval      sz_histype = histype
     c                   eval      *in80 = *off
     c                   exsr      $_hist_ck1
     c                   eval      @returned = *zeros
     c                   eval      key_press = enter_key
     c                   endif
      **** Right adjust
      * Get Product Right adjust info.
     c                   eval      ta_comp    = lda_comp
     c                   eval      ta_codetyp = 'APP'
     c                   eval      ta_codeval = 'PRODUCT   RIGHT_ADJ '
     c                   eval      rightadj = *off

     c     ta_key        chain     rk_tablcod                         26
     c                   if        *in26 = *off
     c                   if        ta_flag1 = 1
     c                   eval      rightadj = *on
     c                   z-add     ta_number1    prodlen
     c                   else
     c                   eval      rightadj = *off
     c                   z-add     0             prodlen
     c                   endif
     c                   else
     c                   eval      rightadj = *off
     c                   z-add     0             prodlen
     c                   endif

      **** Right adjust
     c                   endsr

      * ////////////////////////////////////////////////////// Edit supplier

     c     $_edt_supl    begsr

      * set read indicator off
     c                   eval      *in19 = *off
     c                   eval      errors = *off

      * get from supplier record
     c     sp_frke       setll     rk_suplier
     c     sp_frke       reade(n)  rk_suplier                             19

      *     supplier record not found, send back error
     c                   if        *in19
     c                   eval      errors    = *on
     c                   eval      send_error = *on
     c                   eval      *in56     = *on
     c                   eval      *in61     = *off
     c                   eval      @msg_id   = 'K3_2000'
     c                   eval      @msg_text = zz_frsupl
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   endif

     c                   if        errors = *off
      * get to supplier record
     c                   eval      *in20    = *off
     c     sp_toke       setll     rk_suplier
     c     sp_toke       reade(n)  rk_suplier                             20

      *     supplier record not found, send back error
     c                   if        *in20
     c                   eval      errors    = *on
     c                   eval      send_error = *on
     c                   eval      *in57     = *on
     c                   eval      *in61     = *off
     c                   eval      @msg_id   = 'K3_2000'
     c                   eval      @msg_text = zz_tosupl
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   endif
     c                   endif


     c                   endsr

      * ////////////////////////////////////////////////////// Edit location

     c     $_edt_locn    begsr

      * set read indicator off
     c                   eval      errors = *off

     c                   if        zz_frlocn <> *blanks
     c     lc_frkey      chain(n)  rk_locatns                         15

      *     supplier record not found, send back error
     c                   if        *in15
     c                   eval      errors    = *on
     c                   eval      send_error = *on
     c                   eval      *in58     = *on
     c                   eval      *in61     = *off
     c                   eval      @msg_id   = 'K3_8020'
     c                   eval      @msg_text = zz_frlocn
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   endif
     c                   endif

     c                   if        (errors = *off) and
     c                             (zz_tolocn <> *blanks)
      * get to supplier record
     c     lc_tokey      chain(n)  rk_locatns                         22

      *     supplier record not found, send back error
     c                   if        *in22
     c                   eval      errors    = *on
     c                   eval      send_error = *on
     c                   eval      *in59     = *on
     c                   eval      *in61     = *off
     c                   eval      @msg_id   = 'K3_8020'
     c                   eval      @msg_text = zz_tolocn
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   endif
     c                   endif

     c                   if        (errors = *off) and
     c                             (zz_frlocn = *blanks) and
     c                             (zz_tolocn <> *blanks)
     c                   eval      errors    = *on
     c                   eval      *in58     = *on
     c                   eval      *in61     = *off
     c                   eval      @msg_id   = 'K3_9999'
     c                   eval      @msg_text = 'from loc blank to loc <> blank'
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   endif

     c                   if        (errors = *off) and
     c                             (zz_tolocn = *blanks) and
     c                             (zz_frlocn <> *blanks)
     c                   eval      errors    = *on
     c                   eval      *in58     = *on
     c                   eval      *in61     = *off
     c                   eval      @msg_id   = 'K3_9999'
     c                   eval      @msg_text = 'to loc blank from loc <> blank'
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   endif


     c                   endsr

      * ////////////////////////////////////////////////////// Edit product

     c     $_edt_prod    begsr

      * set read indicator off
     c                   eval      errors = *off
     c                   eval      *in27 = *off

     c                   if        zz_frlocn = *blanks
     c     pr_frke       setll     r1_product
     c                   dou       *in27 = *on or
     c                             pr_supl = zz_frsupl
     c     pr_frke       reade(n)  r1_product                             27
     c                   enddo
     c                   else
     c     pr_frkey      chain(n)  rk_product                         27
     c                   endif

      *     supplier record not found, send back error
     c                   if        *in27
     c                   eval      errors    = *on
     c                   eval      send_error = *on
     c                   eval      *in60     = *on
     c                   eval      *in61     = *off
     c                   eval      @msg_id   = 'K3_3080'
     c                   eval      @msg_text = zz_frprod
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   else
     c                   eval      zz_forcast = pr_forcast * zz_multply
     c                   eval      zz_fordevp = pr_fordevp
     c                   eval      zz_forserr = pr_forserr
     c                   eval      zz_accsale = pr_accsale * zz_multply
     c                   eval      zz_accouts = pr_accouts * zz_multply
     c***********        eval      zz_accdem  = pr_accdem
     c                   eval      zz_accdem  = zz_accsale + zz_accouts
     c                   eval      zz_fstslyr = pr_fstslyr
     c                   eval      zz_fstslpr = pr_fstslpr
     c                   eval      xx_forcyr  = pr_forcyr
     c                   eval      xx_forcper = pr_forcper
     c                   eval      zz_seasonl = pr_seasonl
     c                   eval      zz_frdelt  = pr_deltcnt
     c                   eval      zz_frbuyr  = pr_buyr
     c                   eval      zz_frforci = pr_forcint
     c                   eval      zz_frdesc1 = pr_desc1
     c                   eval      zz_frdesc2 = pr_desc2
     c                   eval      sz_frsupl  = pr_supl
     c                   eval      sz_frsub   = pr_suplsub
     c                   eval      sz_frprod  = pr_prod
     c                   eval      sz_usrstat = *blanks
     c                   eval      sz_frezend = *blanks
     c                   eval      sz_setfore = '0'
     c                   endif

     c                   if        errors = *off
     c                   eval      *in28 = *off
      * get to supplier record
     c                   if        zz_tolocn = *blanks
     c     pr_toke       setll     r1_product
     c                   dou       *in28 = *on or
     c                             pr_supl = zz_tosupl
     c     pr_toke       reade(n)  r1_product                             28
     c                   enddo
     c                   else
     c     pr_tokey      chain(n)  rk_product                         28
     c                   endif

      *     supplier record not found, send back error
     c                   if        *in28
     c                   eval      errors    = *on
     c                   eval      send_error = *on
     c                   eval      *in61     = *on
     c                   eval      @msg_id   = 'K3_3080'
     c                   eval      @msg_text = zz_toprod
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   else
     c                   eval      zz_forcyr  = pr_forcyr
     c                   eval      zz_forcper = pr_forcper
     c                   eval      zz_tobuyr  = pr_buyr
     c                   eval      zz_toforci = pr_forcint
     c                   eval      zz_todesc1 = pr_desc1
     c                   eval      zz_todesc2 = pr_desc2
     c                   eval      zz_todelt  = pr_deltcnt
     c                   eval      sz_tosupl  = pr_supl
     c                   eval      sz_tosub   = pr_suplsub
     c                   eval      sz_toprod  = pr_prod
     c                   endif
     c                   endif

     c                   if        errors = *off
     c                   if        zz_frforci <> zz_toforci and
     c                             add = *on
     c                   eval      errors    = *on
     c                   eval      *in61     = *on
     c                   eval      @msg_id   = 'K3_9999'
     c                   eval      @msg_text = 'Forecast types not the same'
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c                   endif
     c                   endif

     c                   if        errors = *off
     c                   if        zz_frforci <> zz_toforci and
     c                             replace = *on
     c                   eval      diff19    = *on
     c                   endif
     c                   endif




     c                   endsr

      * /////////////////////////////////////////////// get history

     c     $_hist_ck1    begsr
     c                   eval      ta_comp    = comp
     c                   eval      ta_codetyp = 'PHT'
     c                   movel     zz_histype    ta_codeval
     c     ta_key        chain     rk_tablcod                         14
     c                   movel     ta_codeds1    zz_histdsc
     c                   endsr
      * /////////////////////////////////////////////// get history

     c     $_hist_chk    begsr
     c                   if        diff19 = *on
     c                   exsr      $_fix_hist
     c                   endif
     c                   eval      domany = *off
     c                   eval      addmsg = *on
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks)
     c                   eval      *in64 = *off
     c     lc_keyx       setll     rk_locatns
     c                   read(n)   rk_locatns                             64
     c                   eval      *in64 = *off
     c     lc_keyy       setll     rk_locatns
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_frlocn = lc_locn
     c                   eval      zz_tolocn = lc_locn
     c                   endif
     c                   endif
      *     edit location entry
     c                   exsr      $_edt_locn
      *     edit supplier entry
     c                   if        errors = *off
     c                   exsr      $_edt_supl
     c                   endif
      *     edit product entry
     c                   if        errors = *off
     c                   exsr      $_edt_prod
     c                   if        (errors = *on) and
     c                             (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks)
     c                   dou       errors = *off or
     c                             *in64 = *on
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks) and
     c                             (errors = *on)
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_frlocn = lc_locn
     c                   eval      zz_tolocn = lc_locn
     c                   eval      errors = *off
     c                   exsr      $_edt_prod
     c                   endif
     c                   endif
     c                   enddo
     c                   endif
     c                   endif
      *     edit history type entry
     c                   if        errors = *off
     c                   exsr      $_edt_type
     c                   endif
     c                   eval      sz_frsupl = zz_frsupl
     c                   eval      sz_frsub  = zz_frsub
     c                   eval      sz_frprod = zz_frprod
     c                   eval      sz_tosupl = zz_tosupl
     c                   eval      sz_tosub  = zz_tosub
     c                   eval      sz_toprod = zz_toprod
     c                   eval      sz_histype = zz_histype
     c                   eval      sz_usrstat = zz_usrstat
     c                   eval      sz_frezend = zz_frezend
     c                   eval      sz_setfore = zz_setfore
     c                   if        errors = *off
     c                   if        zz_multply = 0
     c                   eval      zz_multply = 1.0000
     c                   endif
     c                   if        zz_frforci < 52
     c     *hival        setll     rk_prodhis
     c     ph_frkey      setll     rk_prodhis
     c     ph_frkey      reade(n)  rk_prodhis                             02

     c                   dow       *in02 = *off


     c                   eval      per_hist(01) = ph_per01  * zz_multply
     c                   eval      per_hist(02) = ph_per02  * zz_multply
     c                   eval      per_hist(03) = ph_per03  * zz_multply
     c                   eval      per_hist(04) = ph_per04  * zz_multply
     c                   eval      per_hist(05) = ph_per05  * zz_multply
     c                   eval      per_hist(06) = ph_per06  * zz_multply
     c                   eval      per_hist(07) = ph_per07  * zz_multply
     c                   eval      per_hist(08) = ph_per08  * zz_multply
     c                   eval      per_hist(09) = ph_per09  * zz_multply
     c                   eval      per_hist(10) = ph_per10  * zz_multply
     c                   eval      per_hist(11) = ph_per11  * zz_multply
     c                   eval      per_hist(12) = ph_per12  * zz_multply
     c                   eval      per_hist(13) = ph_per13  * zz_multply
     c                   eval      xx_year     = ph_year
     c                   exsr      $_hist_chg
     c                   eval      xx_year = xx_year - 1
     c     ph_frkey1     chain(n)  rk_prodhis                         02
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks) and
     c                             (*in02 = *on)
     c                   dou       *in02 = *off or
     c                             *in64 = *on
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_frlocn = lc_locn
     c                   eval      zz_tolocn = lc_locn
     c                   eval      *in02 = *off
     c                   if        pr_forcint < 52
     c     *hival        setll     rk_prodhis
     c     ph_frkey      setll     rk_prodhis
     c     ph_frkey      reade(n)  rk_prodhis                             02
     c                   endif
     c                   eval      domany = *off
     c                   endif
     c                   enddo
     c                   if        *in64 = *on
     c                   eval      domany = *on
     c                   eval      zz_tolocn = *blanks
     c                   eval      zz_frlocn = *blanks
     c                   eval      zl_tolocn = 'All'
     c                   eval      zl_frlocn = 'All'
     c                   eval      *in02 = *on
     c                   endif
     c                   else
     c                   eval      domany = *on
     c                   endif
     c                   enddo
     c                   eval      add = *off
     c                   eval      replace = *off
     c                   endif

     c                   if        zz_frforci = 52
     c     *hival        setll     rk_prodh52
     c     ph_frkey      setll     rk_prodh52
     c     ph_frkey      reade(n)  rk_prodh52                             02

     c                   dow       *in02 = *off


     c                   eval      per_histw(01) = pw_per01 * zz_multply
     c                   eval      per_histw(02) = pw_per02 * zz_multply
     c                   eval      per_histw(03) = pw_per03 * zz_multply
     c                   eval      per_histw(04) = pw_per04 * zz_multply
     c                   eval      per_histw(05) = pw_per05 * zz_multply
     c                   eval      per_histw(06) = pw_per06 * zz_multply
     c                   eval      per_histw(07) = pw_per07 * zz_multply
     c                   eval      per_histw(08) = pw_per08 * zz_multply
     c                   eval      per_histw(09) = pw_per09 * zz_multply
     c                   eval      per_histw(10) = pw_per10 * zz_multply
     c                   eval      per_histw(11) = pw_per11 * zz_multply
     c                   eval      per_histw(12) = pw_per12 * zz_multply
     c                   eval      per_histw(13) = pw_per13 * zz_multply
     c                   eval      per_histw(14) = pw_per14 * zz_multply
     c                   eval      per_histw(15) = pw_per15 * zz_multply
     c                   eval      per_histw(16) = pw_per16 * zz_multply
     c                   eval      per_histw(17) = pw_per17 * zz_multply
     c                   eval      per_histw(18) = pw_per18 * zz_multply
     c                   eval      per_histw(19) = pw_per19 * zz_multply
     c                   eval      per_histw(20) = pw_per20 * zz_multply
     c                   eval      per_histw(21) = pw_per21 * zz_multply
     c                   eval      per_histw(22) = pw_per22 * zz_multply
     c                   eval      per_histw(23) = pw_per23 * zz_multply
     c                   eval      per_histw(24) = pw_per24 * zz_multply
     c                   eval      per_histw(25) = pw_per25 * zz_multply
     c                   eval      per_histw(26) = pw_per26 * zz_multply
     c                   eval      per_histw(27) = pw_per27 * zz_multply
     c                   eval      per_histw(28) = pw_per28 * zz_multply
     c                   eval      per_histw(29) = pw_per29 * zz_multply
     c                   eval      per_histw(30) = pw_per30 * zz_multply
     c                   eval      per_histw(31) = pw_per31 * zz_multply
     c                   eval      per_histw(32) = pw_per32 * zz_multply
     c                   eval      per_histw(33) = pw_per33 * zz_multply
     c                   eval      per_histw(34) = pw_per34 * zz_multply
     c                   eval      per_histw(35) = pw_per35 * zz_multply
     c                   eval      per_histw(36) = pw_per36 * zz_multply
     c                   eval      per_histw(37) = pw_per37 * zz_multply
     c                   eval      per_histw(38) = pw_per38 * zz_multply
     c                   eval      per_histw(39) = pw_per39 * zz_multply
     c                   eval      per_histw(40) = pw_per40 * zz_multply
     c                   eval      per_histw(41) = pw_per41 * zz_multply
     c                   eval      per_histw(42) = pw_per42 * zz_multply
     c                   eval      per_histw(43) = pw_per43 * zz_multply
     c                   eval      per_histw(44) = pw_per44 * zz_multply
     c                   eval      per_histw(45) = pw_per45 * zz_multply
     c                   eval      per_histw(46) = pw_per46 * zz_multply
     c                   eval      per_histw(47) = pw_per47 * zz_multply
     c                   eval      per_histw(48) = pw_per48 * zz_multply
     c                   eval      per_histw(49) = pw_per49 * zz_multply
     c                   eval      per_histw(50) = pw_per50 * zz_multply
     c                   eval      per_histw(51) = pw_per51 * zz_multply
     c                   eval      per_histw(52) = pw_per52 * zz_multply
     c                   eval      xx_year     = pw_year
     c                   exsr      $_hist_chg
     c                   eval      xx_year = xx_year - 1
     c     ph_frkey1     chain(n)  rk_prodh52                         02
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks) and
     c                             (*in02 = *on)
     c                   dou       *in02 = *off or
     c                             *in64 = *on
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_frlocn = lc_locn
     c                   eval      zz_tolocn = lc_locn
     c                   eval      *in02 = *off
     c                   if        pr_forcint = 52
     c     *hival        setll     rk_prodh52
     c     ph_frkey      setll     rk_prodh52
     c     ph_frkey      reade(n)  rk_prodh52                             02
     c                   endif
     c                   eval      domany = *off
     c                   endif
     c                   enddo
     c                   if        *in64 = *on
     c                   eval      domany = *on
     c                   eval      zz_tolocn = *blanks
     c                   eval      zz_frlocn = *blanks
     c                   eval      zl_tolocn = 'All'
     c                   eval      zl_frlocn = 'All'
     c                   eval      *in02 = *on
     c                   endif
     c                   else
     c                   eval      domany = *on
     c                   endif
     c                   enddo
     c                   eval      add = *off
     c                   eval      replace = *off
     c                   endif
     c                   else
     c                   eval      add = *off
     c                   eval      replace = *off
     c                   endif

     c                   eval      addmsg = *off
     c                   eval      diff19 = *off
     c                   endsr

      * /////////////////////////////////////////////// chg history

     c     $_hist_chg    begsr

     c                   if        add = *on
     c                   eval      *in81 = *on
     c                   endif
     c                   if        replace = *on
     c                   eval      *in81 = *off
     c                   endif

     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks) or
     c                             (sz_frlocn <> *blanks) and
     c                             (sz_tolocn <> *blanks)
      *     edit product entry
     c                   exsr      $_edt_prod
     c                   if        (errors = *off) and
     c                             (zz_frforci = zz_toforci)
     c                   if        (add = *on) or
     c                             (replace = *on)
     c                   if        pr_forcint < 52
     c     ph_tokey      chain     rk_prodhis                         03
     c                   if        replace = *on
     c                   eval      ph_per01     = per_hist(01)
     c                   eval      ph_per02     = per_hist(02)
     c                   eval      ph_per03     = per_hist(03)
     c                   eval      ph_per04     = per_hist(04)
     c                   eval      ph_per05     = per_hist(05)
     c                   eval      ph_per06     = per_hist(06)
     c                   eval      ph_per07     = per_hist(07)
     c                   eval      ph_per08     = per_hist(08)
     c                   eval      ph_per09     = per_hist(09)
     c                   eval      ph_per10     = per_hist(10)
     c                   eval      ph_per11     = per_hist(11)
     c                   eval      ph_per12     = per_hist(12)
     c                   eval      ph_per13     = per_hist(13)
     c                   endif
     c                   if        add = *on and
     c                             *in03 = *on
     c                   eval      ph_per01     = per_hist(01)
     c                   eval      ph_per02     = per_hist(02)
     c                   eval      ph_per03     = per_hist(03)
     c                   eval      ph_per04     = per_hist(04)
     c                   eval      ph_per05     = per_hist(05)
     c                   eval      ph_per06     = per_hist(06)
     c                   eval      ph_per07     = per_hist(07)
     c                   eval      ph_per08     = per_hist(08)
     c                   eval      ph_per09     = per_hist(09)
     c                   eval      ph_per10     = per_hist(10)
     c                   eval      ph_per11     = per_hist(11)
     c                   eval      ph_per12     = per_hist(12)
     c                   eval      ph_per13     = per_hist(13)
     c                   endif
     c                   if        add = *on and
     c                             *in03 = *off
     c                   eval      ph_per01 = ph_per01 + per_hist(01)
     c                   eval      ph_per02 = ph_per02 + per_hist(02)
     c                   eval      ph_per03 = ph_per03 + per_hist(03)
     c                   eval      ph_per04 = ph_per04 + per_hist(04)
     c                   eval      ph_per05 = ph_per05 + per_hist(05)
     c                   eval      ph_per06 = ph_per06 + per_hist(06)
     c                   eval      ph_per07 = ph_per07 + per_hist(07)
     c                   eval      ph_per08 = ph_per08 + per_hist(08)
     c                   eval      ph_per09 = ph_per09 + per_hist(09)
     c                   eval      ph_per10 = ph_per10 + per_hist(10)
     c                   eval      ph_per11 = ph_per11 + per_hist(11)
     c                   eval      ph_per12 = ph_per12 + per_hist(12)
     c                   eval      ph_per13 = ph_per13 + per_hist(13)
     c                   endif
     c                   if        *in03 = *off
     c                   eval      ph_lastupd = lc_sysdate
     c                   update    rk_prodhis
     c                   else
     c                   eval      ph_lastupd = lc_sysdate
     c                   eval      ph_birth   = lc_sysdate
     c                   eval      ph_comp    = comp
     c                   eval      ph_locn    = zz_tolocn
     c                   eval      ph_supl    = zz_tosupl
     c                   eval      ph_suplsub = zz_tosub
     c                   eval      ph_suplusr = pr_suplusr
     c                   eval      ph_suplusb = pr_suplusb
     c                   eval      ph_prod    = zz_toprod
     c                   eval      ph_year    = xx_year
     c                   eval      ph_forcint = zz_frforci
     c                   write     rk_prodhis
     c                   endif

     c                   if        domany = *off
     c                   call      'K3S_M090'
     c                   parm                    time_stamp
     c                   move      time_stamp    time
     c                   move      time_stamp    date
     c     pr_tokey      chain     rk_product                         27
     c     pq_tokey      chain     rk_prodsoq                         17
     c                   if        *in17 = *on
     c     pq_tokey1     chain     rk_prodsoq                         17
     c                   endif
     c     so_tokey      chain     rk_suplsoq                         16
     c                   if        *in16 = *on
     c     so_tokey1     chain     rk_suplsoq                         16
     c                   endif
     c                   eval      pf_comp = comp
     c                   eval      pf_locn = pr_locn
     c                   eval      pf_supl = pr_supl
     c                   eval      pf_suplsub = pr_suplsub
     c                   eval      pf_prod = pr_prod
     c                   eval      pf_chgtype = 'Y'
     c                   if        replace = *on and
     c                             diff19 = *on
     c                   eval      pf_chgtype = 'Z'
     c                   endif
     c                   if        updtype = 'O'
     c                   eval      pf_user = zz_user
     c                   eval      pf_workstn = wrk_statn
     c                   move      *blanks       pf_chgdesc
     c                   if        add = *on
     c                   movel     'User add his'pf_chgdesc
     c                   move      'tory    '    pf_chgdesc
     c                   endif
     c                   if        replace = *on
     c                   movel     'User replace'pf_chgdesc
     c                   move      ' history'    pf_chgdesc
     c                   endif
     c                   endif
     c                   if        updtype = 'B'
     c                   eval      pf_user = 'NIGHT JOB '
     c                   eval      pf_workstn = 'NIGHT JOB '
     c                   move      *blanks       pf_chgdesc
     c                   if        add = *on
     c                   movel     'Product link'pf_chgdesc
     c                   move      ' add    '    pf_chgdesc
     c                   endif
     c                   if        replace = *on
     c                   movel     'Product link'pf_chgdesc
     c                   move      ' replace'    pf_chgdesc
     c                   endif
     c                   endif
     c                   eval      pf_program = zz_program
     c                   eval      pf_birth = lc_sysdate
     c                   move      time          pf_birthtm
     c                   if        add = *on
     c                   eval      zz_forcast = zz_forcast + pr_forcast
     c                   if        zz_frforci = zz_toforci and
     c                             zz_forcyr = xx_forcyr and
     c                             zz_forcper = xx_forcper or
     c                             zz_frforci <> zz_toforci
     c                   eval      zz_accsale = zz_accsale + pr_accsale
     c                   eval      zz_accouts = zz_accouts + pr_accouts
     c                   eval      zz_accdem  = zz_accdem  + pr_accdem
     c                   endif
     c                   endif
     c                   eval      pf_avgdiff = zz_forcast - pr_forcast
     c                   eval      pf_avgbef  = pr_forcast
     c                   eval      pf_avgaft  = zz_forcast
     c                   eval      pf_devpbef = pr_fordevp
     c                   eval      pf_devpaft = zz_fordevp
     c                   eval      pf_seasbef = pr_seasonl
     c                   eval      pf_seasaft = zz_seasonl
     c                   eval      pf_statbef = pr_usrstat
     c                   eval      pf_stataft = pr_usrstat
     c                   eval      pf_sysstat = pr_sysstat
     c                   eval      pr_seasonl = zz_seasonl
     c                   if        zz_seasonl <> *blanks
     c                   eval      pr_seassrc = 'U'
     c                   eval      pr_seasact = lc_sysdate
     c                   else
     c                   eval      pr_seassrc = ' '
     c                   move      '0001-01-01'  pr_seasact
     c                   endif
     c                   eval      pr_fordevp = zz_fordevp
     c                   eval      pq_forcast = zz_forcast
     c                   eval      pr_forcast = zz_forcast
     c                   eval      pr_forserr = 0
     c                   if        zz_frforci = zz_toforci and
     c                             zz_forcyr = xx_forcyr and
     c                             zz_forcper = xx_forcper or
     c                             zz_frforci <> zz_toforci
     c                   eval      pr_accsale = zz_accsale
     c                   eval      pr_accouts = zz_accouts
     c                   endif
     c                   if        zz_fstslyr < pr_fstslyr and
     c                             zz_fstslyr <> 0
     c                   eval      pr_fstslyr = zz_fstslyr
     c                   eval      pr_fstslpr = zz_fstslpr
     c                   endif
     c                   if        (zz_fstslyr = pr_fstslyr) and
     c                             (zz_fstslpr < pr_fstslpr)
     c                   eval      pr_fstslpr = zz_fstslpr
     c                   endif
     c                   if        (pr_fstslyr = 0) and
     c                             (pr_fstslpr = 0)
     c                   eval      pr_fstslyr = zz_fstslyr
     c                   eval      pr_fstslpr = zz_fstslpr
     c                   endif
     c                   eval      pr_forchg = 'Y'
     c                   if        replace = *on and
     c                             diff19 = *on
     c                   eval      pr_forchg = 'Z'
     c                   endif
     c                   if        zz_frforci = zz_toforci and
     c                             zz_forcyr = xx_forcyr and
     c                             zz_forcper = xx_forcper or
     c                             zz_frforci <> zz_toforci
     c                   eval      pr_accdem  = zz_accdem
     c                   endif
     c                   eval      pr_formanl = lc_sysdate
     c                   eval      pr_lastupd = lc_sysdate
     c**************     if        pq_chkchg = 0
     c**************     eval      pq_chkchg = 1
     c**************     eval      so_chkchg = so_chkchg + 1
     c**************     endif
     c                   eval      *in80 = *on
     c                   eval      zl_frlocn  = zz_frlocn
     c                   eval      zl_frsupl  = zz_frsupl
     c                   eval      zl_frsub   = zz_frsub
     c                   eval      zl_frprod  = zz_frprod
     c                   eval      zl_frdesc1 = zz_frdesc1
     c                   eval      zl_tolocn  = zz_tolocn
     c                   eval      zl_tosupl  = zz_tosupl
     c                   eval      zl_tosub   = zz_tosub
     c                   eval      zl_toprod  = zz_toprod
     c                   eval      zl_todesc1 = zz_todesc1
     c                   eval      zl_multply = zz_multply
     c                   eval      pr_ansale$ = pr_forcast * pr_sales *
     c                                          pr_forcint
     c                   eval      pr_ansaleu = pr_forcast * pr_forcint
      **           new products will become Regular (if forecast exists)
     c                   if        pr_sysstat = 'N' and pr_forcast > 0
     c                   eval      pr_sysstat = 'R'
     c                   eval      pq_chknew = 0
     c                   eval      so_chknew = so_chknew - 1
     c*************      if        pq_chkchg = 0
     c*************      eval      pq_chkchg = 1
     c*************      eval      so_chkchg = so_chkchg + 1
     c*************      endif
     c                   endif
     c/copy k3s_c160
     c                   update    rk_product

     c                   clear                   savebirth
     c     pr_frkey      chain     rk_product
     c                   if        %found
     c                   if        pr_rfbirth <>  pr_birth or
     c                             pr_rfbirth <  pr_birth
     c                   eval      savebirth = pr_rfbirth
     c                   endif
     c                   endif

     c     pr_tokey      chain     rk_product

     c*****              if        savebirth <> '0001-01-01'
     c*****              eval      pr_rfbirth = savebirth
     c******             update    rk_product
     c*******            endif

     c                   if        *in17 = *off
     c                   update    rk_prodsoq
     c                   endif
     c                   if        *in16 = *off
     c                   update    rk_suplsoq
     c                   endif
     c                   exsr      $_wrt_prod
     c                   exsr      $_edt_stat
     c                   endif
     c                   endif

     c                   if        pr_forcint = 52

     c     ph_tokey      chain     rk_prodh52                         03
     c                   if        replace = *on
     c                   eval      pw_per01     = per_histw(01)
     c                   eval      pw_per02     = per_histw(02)
     c                   eval      pw_per03     = per_histw(03)
     c                   eval      pw_per04     = per_histw(04)
     c                   eval      pw_per05     = per_histw(05)
     c                   eval      pw_per06     = per_histw(06)
     c                   eval      pw_per07     = per_histw(07)
     c                   eval      pw_per08     = per_histw(08)
     c                   eval      pw_per09     = per_histw(09)
     c                   eval      pw_per10     = per_histw(10)
     c                   eval      pw_per11     = per_histw(11)
     c                   eval      pw_per12     = per_histw(12)
     c                   eval      pw_per13     = per_histw(13)
     c                   eval      pw_per14     = per_histw(14)
     c                   eval      pw_per15     = per_histw(15)
     c                   eval      pw_per16     = per_histw(16)
     c                   eval      pw_per17     = per_histw(17)
     c                   eval      pw_per18     = per_histw(18)
     c                   eval      pw_per19     = per_histw(19)
     c                   eval      pw_per20     = per_histw(20)
     c                   eval      pw_per21     = per_histw(21)
     c                   eval      pw_per22     = per_histw(22)
     c                   eval      pw_per23     = per_histw(23)
     c                   eval      pw_per24     = per_histw(24)
     c                   eval      pw_per25     = per_histw(25)
     c                   eval      pw_per26     = per_histw(26)
     c                   eval      pw_per27     = per_histw(27)
     c                   eval      pw_per28     = per_histw(28)
     c                   eval      pw_per29     = per_histw(29)
     c                   eval      pw_per30     = per_histw(30)
     c                   eval      pw_per31     = per_histw(31)
     c                   eval      pw_per32     = per_histw(32)
     c                   eval      pw_per33     = per_histw(33)
     c                   eval      pw_per34     = per_histw(34)
     c                   eval      pw_per35     = per_histw(35)
     c                   eval      pw_per36     = per_histw(36)
     c                   eval      pw_per37     = per_histw(37)
     c                   eval      pw_per38     = per_histw(38)
     c                   eval      pw_per39     = per_histw(39)
     c                   eval      pw_per40     = per_histw(40)
     c                   eval      pw_per41     = per_histw(41)
     c                   eval      pw_per42     = per_histw(42)
     c                   eval      pw_per43     = per_histw(43)
     c                   eval      pw_per44     = per_histw(44)
     c                   eval      pw_per45     = per_histw(45)
     c                   eval      pw_per46     = per_histw(46)
     c                   eval      pw_per47     = per_histw(47)
     c                   eval      pw_per48     = per_histw(48)
     c                   eval      pw_per49     = per_histw(49)
     c                   eval      pw_per50     = per_histw(50)
     c                   eval      pw_per51     = per_histw(51)
     c                   eval      pw_per52     = per_histw(52)
     c                   endif
     c                   if        add = *on and
     c                             *in03 = *on
     c                   eval      pw_per01     = per_histw(01)
     c                   eval      pw_per02     = per_histw(02)
     c                   eval      pw_per03     = per_histw(03)
     c                   eval      pw_per04     = per_histw(04)
     c                   eval      pw_per05     = per_histw(05)
     c                   eval      pw_per06     = per_histw(06)
     c                   eval      pw_per07     = per_histw(07)
     c                   eval      pw_per08     = per_histw(08)
     c                   eval      pw_per09     = per_histw(09)
     c                   eval      pw_per10     = per_histw(10)
     c                   eval      pw_per11     = per_histw(11)
     c                   eval      pw_per12     = per_histw(12)
     c                   eval      pw_per13     = per_histw(13)
     c                   eval      pw_per14     = per_histw(14)
     c                   eval      pw_per15     = per_histw(15)
     c                   eval      pw_per16     = per_histw(16)
     c                   eval      pw_per17     = per_histw(17)
     c                   eval      pw_per18     = per_histw(18)
     c                   eval      pw_per19     = per_histw(19)
     c                   eval      pw_per20     = per_histw(20)
     c                   eval      pw_per21     = per_histw(21)
     c                   eval      pw_per22     = per_histw(22)
     c                   eval      pw_per23     = per_histw(23)
     c                   eval      pw_per24     = per_histw(24)
     c                   eval      pw_per25     = per_histw(25)
     c                   eval      pw_per26     = per_histw(26)
     c                   eval      pw_per27     = per_histw(27)
     c                   eval      pw_per28     = per_histw(28)
     c                   eval      pw_per29     = per_histw(29)
     c                   eval      pw_per30     = per_histw(30)
     c                   eval      pw_per31     = per_histw(31)
     c                   eval      pw_per32     = per_histw(32)
     c                   eval      pw_per33     = per_histw(33)
     c                   eval      pw_per34     = per_histw(34)
     c                   eval      pw_per35     = per_histw(35)
     c                   eval      pw_per36     = per_histw(36)
     c                   eval      pw_per37     = per_histw(37)
     c                   eval      pw_per38     = per_histw(38)
     c                   eval      pw_per39     = per_histw(39)
     c                   eval      pw_per40     = per_histw(40)
     c                   eval      pw_per41     = per_histw(41)
     c                   eval      pw_per42     = per_histw(42)
     c                   eval      pw_per43     = per_histw(43)
     c                   eval      pw_per44     = per_histw(44)
     c                   eval      pw_per45     = per_histw(45)
     c                   eval      pw_per46     = per_histw(46)
     c                   eval      pw_per47     = per_histw(47)
     c                   eval      pw_per48     = per_histw(48)
     c                   eval      pw_per49     = per_histw(49)
     c                   eval      pw_per50     = per_histw(50)
     c                   eval      pw_per51     = per_histw(51)
     c                   eval      pw_per52     = per_histw(52)
     c                   endif
     c                   if        add = *on and
     c                             *in03 = *off
     c                   eval      pw_per01 = pw_per01 + per_histw(01)
     c                   eval      pw_per02 = pw_per02 + per_histw(02)
     c                   eval      pw_per03 = pw_per03 + per_histw(03)
     c                   eval      pw_per04 = pw_per04 + per_histw(04)
     c                   eval      pw_per05 = pw_per05 + per_histw(05)
     c                   eval      pw_per06 = pw_per06 + per_histw(06)
     c                   eval      pw_per07 = pw_per07 + per_histw(07)
     c                   eval      pw_per08 = pw_per08 + per_histw(08)
     c                   eval      pw_per09 = pw_per09 + per_histw(09)
     c                   eval      pw_per10 = pw_per10 + per_histw(10)
     c                   eval      pw_per11 = pw_per11 + per_histw(11)
     c                   eval      pw_per12 = pw_per12 + per_histw(12)
     c                   eval      pw_per13 = pw_per13 + per_histw(13)
     c                   eval      pw_per14 = pw_per14 + per_histw(14)
     c                   eval      pw_per15 = pw_per15 + per_histw(15)
     c                   eval      pw_per16 = pw_per16 + per_histw(16)
     c                   eval      pw_per17 = pw_per17 + per_histw(17)
     c                   eval      pw_per18 = pw_per18 + per_histw(18)
     c                   eval      pw_per19 = pw_per19 + per_histw(19)
     c                   eval      pw_per20 = pw_per20 + per_histw(20)
     c                   eval      pw_per21 = pw_per21 + per_histw(21)
     c                   eval      pw_per22 = pw_per22 + per_histw(22)
     c                   eval      pw_per23 = pw_per23 + per_histw(23)
     c                   eval      pw_per24 = pw_per24 + per_histw(24)
     c                   eval      pw_per25 = pw_per25 + per_histw(25)
     c                   eval      pw_per26 = pw_per26 + per_histw(26)
     c                   eval      pw_per27 = pw_per27 + per_histw(27)
     c                   eval      pw_per28 = pw_per28 + per_histw(28)
     c                   eval      pw_per29 = pw_per29 + per_histw(29)
     c                   eval      pw_per30 = pw_per30 + per_histw(30)
     c                   eval      pw_per31 = pw_per31 + per_histw(31)
     c                   eval      pw_per32 = pw_per32 + per_histw(32)
     c                   eval      pw_per33 = pw_per33 + per_histw(33)
     c                   eval      pw_per34 = pw_per34 + per_histw(34)
     c                   eval      pw_per35 = pw_per35 + per_histw(35)
     c                   eval      pw_per36 = pw_per36 + per_histw(36)
     c                   eval      pw_per37 = pw_per37 + per_histw(37)
     c                   eval      pw_per38 = pw_per38 + per_histw(38)
     c                   eval      pw_per39 = pw_per39 + per_histw(39)
     c                   eval      pw_per40 = pw_per40 + per_histw(40)
     c                   eval      pw_per41 = pw_per41 + per_histw(41)
     c                   eval      pw_per42 = pw_per42 + per_histw(42)
     c                   eval      pw_per43 = pw_per43 + per_histw(43)
     c                   eval      pw_per44 = pw_per44 + per_histw(44)
     c                   eval      pw_per45 = pw_per45 + per_histw(45)
     c                   eval      pw_per46 = pw_per46 + per_histw(46)
     c                   eval      pw_per47 = pw_per47 + per_histw(47)
     c                   eval      pw_per48 = pw_per48 + per_histw(48)
     c                   eval      pw_per49 = pw_per49 + per_histw(49)
     c                   eval      pw_per50 = pw_per50 + per_histw(50)
     c                   eval      pw_per51 = pw_per51 + per_histw(51)
     c                   eval      pw_per52 = pw_per52 + per_histw(52)
     c                   endif
     c                   if        *in03 = *off
     c                   eval      pw_lastupd = lc_sysdate
     c                   update    rk_prodh52
     c                   else
     c                   eval      pw_lastupd = lc_sysdate
     c                   eval      pw_birth   = lc_sysdate
     c                   eval      pw_comp    = comp
     c                   eval      pw_locn    = zz_tolocn
     c                   eval      pw_supl    = zz_tosupl
     c                   eval      pw_suplsub = zz_tosub
     c                   eval      pw_suplusr = pr_suplusr
     c                   eval      pw_suplusb = pr_suplusb
     c                   eval      pw_prod    = zz_toprod
     c                   eval      pw_year    = xx_year
     c                   eval      pw_forcint = zz_frforci
     c                   write     rk_prodh52
     c                   endif
     c                   if        domany = *off
     c                   call      'K3S_M090'
     c                   parm                    time_stamp
     c                   move      time_stamp    time
     c                   move      time_stamp    date
     c     pr_tokey      chain     rk_product                         27
     c     pq_tokey      chain     rk_prodsoq                         17
     c                   if        *in17 = *on
     c     pq_tokey1     chain     rk_prodsoq                         17
     c                   endif
     c     so_tokey      chain     rk_suplsoq                         16
     c                   if        *in16 = *on
     c     so_tokey1     chain     rk_suplsoq                         16
     c                   endif
     c                   eval      pf_comp = comp
     c                   eval      pf_locn = pr_locn
     c                   eval      pf_supl = pr_supl
     c                   eval      pf_suplsub = pr_suplsub
     c                   eval      pf_prod = pr_prod
     c                   eval      pf_chgtype = 'Y'
     c                   if        replace = *on and
     c                             diff19 = *on
     c                   eval      pf_chgtype = 'Z'
     c                   endif
     c                   if        updtype = 'O'
     c                   eval      pf_user = zz_user
     c                   eval      pf_workstn = wrk_statn
     c                   move      *blanks       pf_chgdesc
     c                   if        add = *on
     c                   movel     'User add his'pf_chgdesc
     c                   move      'tory    '    pf_chgdesc
     c                   endif
     c                   if        replace = *on
     c                   movel     'User replace'pf_chgdesc
     c                   move      ' history'    pf_chgdesc
     c                   endif
     c                   endif
     c                   if        updtype = 'B'
     c                   eval      pf_user = 'NIGHT JOB '
     c                   eval      pf_workstn = 'NIGHT JOB '
     c                   move      *blanks       pf_chgdesc
     c                   if        add = *on
     c                   movel     'Product link'pf_chgdesc
     c                   move      ' add    '    pf_chgdesc
     c                   endif
     c                   if        replace = *on
     c                   movel     'Product link'pf_chgdesc
     c                   move      ' replace'    pf_chgdesc
     c                   endif
     c                   endif
     c                   eval      pf_program = zz_program
     c                   eval      pf_birth = lc_sysdate
     c                   move      time          pf_birthtm
     c                   if        add = *on
     c                   eval      zz_forcast = zz_forcast + pr_forcast
     c                   if        zz_frforci = zz_toforci and
     c                             zz_forcyr = xx_forcyr and
     c                             zz_forcper = xx_forcper or
     c                             zz_frforci <> zz_toforci
     c                   eval      zz_accsale = zz_accsale + pr_accsale
     c                   eval      zz_accouts = zz_accouts + pr_accouts
     c                   eval      zz_accdem  = zz_accdem  + pr_accdem
     c                   endif
     c                   endif
     c                   eval      pf_avgdiff = zz_forcast - pr_forcast
     c                   eval      pf_avgbef  = pr_forcast
     c                   eval      pf_avgaft  = zz_forcast
     c                   eval      pf_devpbef = pr_fordevp
     c                   eval      pf_devpaft = zz_fordevp
     c                   eval      pf_seasbef = pr_seasonl
     c                   eval      pf_seasaft = zz_seasonl
     c                   eval      pf_statbef = pr_usrstat
     c                   eval      pf_stataft = pr_usrstat
     c                   eval      pf_sysstat = pr_sysstat
     c                   eval      pr_seasonl = zz_seasonl
     c                   if        zz_seasonl <> *blanks
     c                   eval      pr_seassrc = 'U'
     c                   eval      pr_seasact = lc_sysdate
     c                   else
     c                   eval      pr_seassrc = ' '
     c                   move      '0001-01-01'  pr_seasact
     c                   endif
     c                   eval      pr_fordevp = zz_fordevp
     c                   eval      pq_forcast = zz_forcast
     c                   eval      pr_forcast = zz_forcast
     c                   eval      pr_forserr = 0
     c                   if        zz_frforci = zz_toforci and
     c                             zz_forcyr = xx_forcyr and
     c                             zz_forcper = xx_forcper or
     c                             zz_frforci <> zz_toforci
     c                   eval      pr_accsale = zz_accsale
     c                   eval      pr_accouts = zz_accouts
     c                   eval      pr_accdem  = zz_accdem
     c                   endif
     c                   if        zz_fstslyr < pr_fstslyr and
     c                             zz_fstslyr <> 0
     c                   eval      pr_fstslyr = zz_fstslyr
     c                   eval      pr_fstslpr = zz_fstslpr
     c                   endif
     c                   if        (zz_fstslyr = pr_fstslyr) and
     c                             (zz_fstslpr < pr_fstslpr)
     c                   eval      pr_fstslpr = zz_fstslpr
     c                   endif
     c                   if        (pr_fstslyr = 0) and
     c                             (pr_fstslpr = 0)
     c                   eval      pr_fstslyr = zz_fstslyr
     c                   eval      pr_fstslpr = zz_fstslpr
     c                   endif
     c                   eval      pr_formanl = lc_sysdate
     c                   eval      pr_forchg = 'Y'
     c                   if        replace = *on and
     c                             diff19 = *on
     c                   eval      pr_forchg = 'Z'
     c                   endif
     c                   eval      pr_lastupd = lc_sysdate
     c*************      if        pq_chkchg = 0
     c*************      eval      pq_chkchg = 1
     c*************      eval      so_chkchg = so_chkchg + 1
     c*************      endif
     c                   eval      *in80 = *on
     c                   eval      zl_frlocn  = zz_frlocn
     c                   eval      zl_frsupl  = zz_frsupl
     c                   eval      zl_frsub   = zz_frsub
     c                   eval      zl_frprod  = zz_frprod
     c                   eval      zl_frdesc1 = zz_frdesc1
     c                   eval      zl_tolocn  = zz_tolocn
     c                   eval      zl_tosupl  = zz_tosupl
     c                   eval      zl_tosub   = zz_tosub
     c                   eval      zl_toprod  = zz_toprod
     c                   eval      zl_todesc1 = zz_todesc1
     c                   eval      zl_multply = zz_multply
     c                   eval      pr_ansale$ = pr_forcast * pr_sales *
     c                                          pr_forcint
     c                   eval      pr_ansaleu = pr_forcast * pr_forcint
      **           new products will become Regular (if forecast exists)
     c                   if        pr_sysstat = 'N' and pr_forcast > 0
     c                   eval      pr_sysstat = 'R'
     c                   eval      pq_chknew = 0
     c                   eval      so_chknew = so_chknew - 1
     c**************     if        pq_chkchg = 0
     c**************     eval      pq_chkchg = 1
     c**************     eval      so_chkchg = so_chkchg + 1
     c**************     endif
     c                   endif
     c/copy k3s_c160
     c                   update    rk_product

     c                   clear                   savebirth
     c     pr_frkey      chain     rk_product
     c                   if        %found
     c                   if        pr_rfbirth <>  pr_birth or
     c                             pr_rfbirth <  pr_birth
     c                   eval      savebirth = pr_rfbirth
     c                   endif
     c                   endif

     c     pr_tokey      chain     rk_product

     c*****              if        savebirth <> '0001-01-01'
     c*****              eval      pr_rfbirth = savebirth
     c****               update    rk_product
     c****               endif

     c                   if        *in17 = *off
     c                   update    rk_prodsoq
     c                   endif
     c                   if        *in16 = *off
     c                   update    rk_suplsoq
     c                   endif
     c                   exsr      $_wrt_prod
     c                   exsr      $_edt_stat
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   eval      zz_modedsp = mode
     c                   eval      *in98 = *off
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   exsr      $_pass_msg
     c                   endif
     c                   endsr

      * //////////////////////////////////////Call appropriate selection Pgm

     c     $_cur_sel     begsr

     c                   if        (*in(tablphtNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_HISTYPE')
     c                   movel     zz_histype    ta_codeval

      * call module to obtain table code file information for type
     c                   call      'K3S_9540'
     c                   parm                    comp
     c                   parm      'PHT'         ta_codetyp
     c                   parm                    ta_codeval
     c                   parm                    send_error
     c                   parm      ' '           blank
     c                   parm      0             flag3_test
     c                   movel     ta_codeval    zz_histype
     c                   endif

     c                   if        (key_press = f04_key) and
     c                             (zz_field = 'ZZ_FRPROD ')
     c                   call      'K3S_9560'
     c                   parm                    comp
     c                   parm                    zz_frsupl
     c                   parm                    zz_frsub
     c                   parm                    zz_frprod
     c                   endif

     c                   if        (key_press = f04_key) and
     c                             (zz_field = 'ZZ_TOPROD ')
     c                   call      'K3S_9560'
     c                   parm                    comp
     c                   parm                    zz_tosupl
     c                   parm                    zz_tosub
     c                   parm                    zz_toprod
     c                   endif

     c                   if        (*in(locatnfNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_FRLOCN ')

      * call module to obtain location numbers and names.
     c                   call      'K3S_9510'
     c                   parm                    comp
     c                   parm                    zz_frlocn
     c                   parm                    send_error
     c                   parm      ' '           blank
     c                   endif


     c                   if        (key_press = f04_key) and
     c                             (zz_field = 'ZZ_FRSUPL ') or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_FRSUB  ')

      * call module to obtain supplier numbers and names.
     c                   call      'K3S_9520'
     c                   parm                    comp
     c                   parm                    lda_buyr
     c                   parm                    zz_frlocn
     c                   parm                    xx_frsupl
     c                   parm                    xx_frsub
     c                   parm                    send_error
     c                   parm      ' '           blank
     c                   if        xx_frsupl <> *blanks
     c                   eval      zz_frsupl = xx_frsupl
     c                   endif
     c                   if        xx_frsub  <> *blanks
     c                   eval      zz_frsub  = xx_frsub
     c                   endif
     c                   endif


     c                   if        (*in(locatntNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_TOLOCN ')

      * call module to obtain location numbers and names.
     c                   call      'K3S_9510'
     c                   parm                    comp
     c                   parm                    zz_tolocn
     c                   parm                    send_error
     c                   parm      '1'           blank
     c                   endif


     c                   if        (key_press = f04_key) and
     c                             (zz_field = 'ZZ_TOSUPL ') or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_TOSUB  ')

      * call module to obtain supplier numbers and names.
     c                   call      'K3S_9520'
     c                   parm                    comp
     c                   parm                    lda_buyr
     c                   parm                    zz_tolocn
     c                   parm                    xx_tosupl
     c                   parm                    xx_tosub
     c                   parm                    send_error
     c                   parm      ' '           blank
     c                   if        xx_tosupl <> *blanks
     c                   eval      zz_tosupl = xx_tosupl
     c                   endif
     c                   if        xx_tosub  <> *blanks
     c                   eval      zz_tosub  = xx_tosub
     c                   endif
     c                   endif

      *  user status
     c                   if        (*in(tablpsuNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_USRSTAT')
     c                   eval      ta_codeval = %trimr(zz_usrstat)

     c                   call      'K3S_9540'
     c                   parm                    comp
     c                   parm      'PSU'         ta_codetyp
     c                   parm                    ta_codeval
     c                   parm                    send_error
     c                   parm      '1'           blank
     c                   parm      0             flag3_test
     c                   eval      zz_usrstat = %trimr(ta_codeval)
     c                   endif


     c                   endsr
      * ////////////////////////////////////////////// Edit hist type

     c     $_edt_type    begsr

      * prime key list to read this company only
     c                   eval      ta_comp    = comp
     c                   eval      ta_codetyp = 'PHT'
     c                   move      *blanks       ta_codeval
     c                   movel     zz_histype    ta_codeval

      * get requested type record in table file
     c     ta_key        chain     rk_tablcod                         14

      *      requested type does not exist
     c                   if        *in(tablphtNOT)
     c                   eval      errors    = *on
     c                   eval      *in62     = *on
     c                   eval      *in61     = *off
     c                   eval      send_error= *on
     c                   eval      @msg_id   = 'K3_9020'
     c                   eval      @msg_text = *blanks
     c                   movel     zz_histype    @msg_text
     c                   if        addmsg = *off
     c                   exsr      $_add_msg
     c                   endif
     c
     c                   else
     c                   movel     ta_codeds1    zz_histdsc
     c                   endif

     c                   endsr
      * ////////////////////////////////////////////// Edit user status

     c     $_edt_sta1    begsr

      * prime key list to read this company only
     c                   eval      ta_comp    = comp
     c                   eval      ta_codetyp = 'PSU'
     c                   eval      ta_codeval = zz_usrstat

      * blank requested type would mean all types
     c                   if        zz_usrstat = ' ' and
     c                             zz_frezend <> *blanks
     c                   eval      errors    = *on
     c                   eval      *in29 = *on
     c                   eval      @msg_id   = 'K3_9999'
     c                   eval      @msg_text = 'Freeze Until MUST be blank     '
     c                   exsr      $_add_msg
     c                   endif
     c                   if        zz_usrstat = *blanks

      * otherwise, validate entry
     c                   else

      * get requested type record in table file
     c     ta_key        chain     rk_tablcod                         18

      *      requested type does not exist
     c                   if        *in(tablpsuNOT)
     c                   eval      errors    = *on
     c                   eval      send_error= *on
     c                   eval      @msg_id   = 'K3_9360'
     c                   eval      @msg_text = zz_usrstat
     c                   exsr      $_add_msg
     c
     c                   endif

     c                   if        errors = *off and
     c                             zz_usrstat = 'F' and
     c                             zz_frezend = *blanks
     c                   eval      errors    = *on
     c                   eval      *in29 = *on
     c                   eval      @msg_id   = 'K3_9999'
     c                   eval      @msg_text = 'Freeze Until MUST be entered   '
     c                   exsr      $_add_msg
     c
     c                   endif

     c                   if        errors = *off and
     c                             zz_usrstat <> 'F' and
     c                             zz_frezend <> *blanks
     c                   eval      errors    = *on
     c                   eval      *in29 = *on
     c                   eval      @msg_id   = 'K3_9999'
     c                   eval      @msg_text = 'Freeze Until MUST be blank     '
     c                   exsr      $_add_msg
     c
     c                   endif

     c                   if        errors = *off and
     c                             zz_usrstat = 'P'
     c                   eval      errors    = *on
     c                   eval      *in30 = *on
     c                   eval      @msg_id   = 'K3_9999'
     c                   eval      @msg_text = 'P status is invalid on copy    '
     c                   exsr      $_add_msg
     c                   else
     c                   eval      *in30 = *off
     c                   endif

     c                   endif
     c                   endsr

      * ////////////////////////////////////////////// Edit user status

     c     $_edt_stat    begsr

     c                   if        zz_usrstat <> *blanks or
     c                             zz_setfore = '1'
     c                   call      'K3S_M090'
     c                   parm                    time_stamp
     c                   move      time_stamp    time
     c                   move      time_stamp    date
     c     pr_frkey      chain     rk_product                         27
     c     pq_frkey      chain     rk_prodsoq                         17
     c     so_frkey      chain     rk_suplsoq                         16
     c                   eval      pf_comp = comp
     c                   eval      pf_locn = pr_locn
     c                   eval      pf_supl = pr_supl
     c                   eval      pf_suplsub = pr_suplsub
     c                   eval      pf_prod = pr_prod
     c                   eval      pf_chgtype = 'Y'
     c                   if        replace = *on and
     c                             diff19 = *on
     c                   eval      pf_chgtype = 'Z'
     c                   endif
     c                   if        updtype = 'O'
     c                   eval      pf_user = zz_user
     c                   eval      pf_workstn = wrk_statn
     c                   move      *blanks       pf_chgdesc
     c                   if        add = *on
     c                   movel     'User add his'pf_chgdesc
     c                   move      'tory    '    pf_chgdesc
     c                   endif
     c                   if        replace = *on
     c                   movel     'User replace'pf_chgdesc
     c                   move      ' history'    pf_chgdesc
     c                   endif
     c                   endif
     c                   if        updtype = 'B'
     c                   eval      pf_user = 'NIGHT JOB '
     c                   eval      pf_workstn = 'NIGHT JOB '
     c                   move      *blanks       pf_chgdesc
     c                   if        add = *on
     c                   movel     'Product link'pf_chgdesc
     c                   move      ' add    '    pf_chgdesc
     c                   endif
     c                   if        replace = *on
     c                   movel     'Product link'pf_chgdesc
     c                   move      ' replace'    pf_chgdesc
     c                   endif
     c                   endif
     c                   eval      pf_program = zz_program
     c                   eval      pf_birth = lc_sysdate
     c                   move      time          pf_birthtm
     c                   if        zz_setfore = '1'
     c                   eval      zz_forcast = 0
     c                   endif
     c                   if        zz_setfore = '1'
     c                   eval      pr_forchg = 'Y'
     c                   endif
     c                   if        replace = *on and
     c                             diff19 = *on
     c                   eval      pr_forchg = 'Z'
     c                   endif
     c                   eval      pr_lastupd = lc_sysdate
     c                   if        zz_setfore = '1' and
     c                             zz_usrstat = *blanks
     c*************      if        pq_chkchg = 0
     c*************      eval      pq_chkchg = 1
     c*************      eval      so_chkchg = so_chkchg + 1
     c*************      endif
     c                   endif
     c                   if        updtype = 'O'
     c                   eval      pf_user = zz_user
     c                   eval      pf_workstn = wrk_statn
     c                   endif
     c                   if        updtype = 'B'
     c                   eval      pf_user = 'NIGHT JOB '
     c                   eval      pf_workstn = 'NIGHT JOB '
     c                   endif
     c                   eval      pf_program = zz_program
     c                   if        zz_setfore = '1'
     c                   eval      pf_avgdiff = zz_forcast - pr_forcast
     c                   eval      pf_avgbef  = pr_forcast
     c                   eval      pr_forcast = zz_forcast
     c                   eval      pr_forserr = 0
     c                   eval      pf_avgaft  = zz_forcast
     c                   eval      pf_devpbef = pr_fordevp
     c                   eval      pf_devpaft = pr_fordevp
     c                   eval      pf_comp = comp
     c                   eval      pf_locn = pr_locn
     c                   eval      pf_supl = pr_supl
     c                   eval      pf_suplsub = pr_suplsub
     c                   eval      pf_seasbef = pr_seasonl
     c                   eval      pf_seasaft = pr_seasonl
     c                   eval      pf_prod = pr_prod
     c                   eval      pf_chgtype = 'U'
     c                   eval      pf_statbef = pr_usrstat
     c                   eval      pf_stataft = pr_usrstat
     c                   eval      pf_sysstat = pr_sysstat
     c                   exsr      $_wrt_prod
     c                   endif
     c                   if        pr_usrstat <> zz_usrstat and
     c                             zz_usrstat <> ' '
     c     pf_birthtm    adddur    1:*s          pf_birthtm
     c                   eval      pf_avgdiff = 0
     c                   eval      pf_avgbef  = pr_forcast
     c                   eval      pf_avgaft  = pr_forcast
     c                   eval      pf_devpbef = pr_fordevp
     c                   eval      pf_devpaft = pr_fordevp
     c                   eval      pf_comp = comp
     c                   eval      pf_locn = pr_locn
     c                   eval      pf_supl = pr_supl
     c                   eval      pf_suplsub = pr_suplsub
     c                   eval      pf_seasbef = pr_seasonl
     c                   eval      pf_seasaft = pr_seasonl
     c                   eval      pf_prod = pr_prod
     c                   eval      pf_chgtype = 'S'
     c                   eval      pf_statbef = pr_usrstat
     c                   eval      pf_stataft = zz_usrstat
     c                   eval      pf_sysstat = pr_sysstat
     c                   exsr      $_wrt_prod
     c                   endif
     c                   if        (pr_usrstat = 'M') and
     c                             (zz_usrstat = 'W')
     c                   eval      pr_forchg = 'S'
     c                   eval      pq_chkmanl = pq_chkmanl - 1
     c                   eval      pq_chkwatc = pq_chkwatc + 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkmanl = so_chkmanl - 1
     c                   eval      so_chkwatc = so_chkwatc + 1
     c************       if        pq_chkchg = 0
     c************       eval      pq_chkchg = 1
     c************       eval      so_chkchg = so_chkchg + 1
     c************       endif
     c                   endif

     c                   if        (pr_usrstat = 'W') and
     c                             (zz_usrstat = 'M')
     c                   eval      pr_forchg = 'S'
     c                   eval      pq_chkmanl = pq_chkmanl + 1
     c                   eval      pq_chkwatc = pq_chkwatc - 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkmanl = so_chkmanl + 1
     c                   eval      so_chkwatc = so_chkwatc - 1
     c*************      if        pq_chkchg = 0
     c*************      eval      pq_chkchg = 1
     c*************      eval      so_chkchg = so_chkchg + 1
     c*************      endif
     c                   endif

     c                   if        (pr_usrstat = ' ') and
     c                             (zz_usrstat = 'W')
     c                   eval      pr_forchg = 'S'
     c                   eval      pq_chkwatc = pq_chkwatc + 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkwatc = so_chkwatc + 1
     c**********         if        pq_chkchg = 0
     c**********         eval      pq_chkchg = 1
     c**********         eval      so_chkchg = so_chkchg + 1
     c**********         endif
     c                   endif

     c                   if        (pr_usrstat = ' ') and
     c                             (zz_usrstat = 'M')
     c                   eval      pr_forchg = 'S'
     c                   eval      pq_chkmanl = pq_chkmanl + 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkmanl = so_chkmanl + 1
     c**********         if        pq_chkchg = 0
     c**********         eval      pq_chkchg = 1
     c**********         eval      so_chkchg = so_chkchg + 1
     c**********         endif
     c                   endif

     c                   if        (pr_usrstat = 'P') and
     c                             (zz_usrstat = 'W')
     c                   eval      pq_chkwatc = pq_chkwatc + 1
     c                   eval      pq_chkprob = pq_chkprob - 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkwatc = so_chkwatc + 1
     c                   eval      so_chkprob = so_chkprob - 1
     c***********        if        pq_chkchg = 0
     c***********        eval      pq_chkchg = 1
     c***********        eval      so_chkchg = so_chkchg + 1
     c***********        endif
     c                   move      '0001-01-01'  pr_probdat
     c                   eval      pr_forchg = 'S'
     c                   endif

     c                   if        (pr_usrstat = 'P') and
     c                             (zz_usrstat = 'M')
     c                   eval      pq_chkmanl = pq_chkmanl + 1
     c                   eval      pq_chkprob = pq_chkprob - 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkmanl = so_chkmanl + 1
     c                   eval      so_chkprob = so_chkprob - 1
     c***********        if        pq_chkchg = 0
     c***********        eval      pq_chkchg = 1
     c***********        eval      so_chkchg = so_chkchg + 1
     c***********        endif
     c                   move      '0001-01-01'  pr_probdat
     c                   eval      pr_forchg = 'S'
     c                   endif


     c                   if        (pr_usrstat = 'W') and
     c                             (zz_usrstat = 'F')
     c                   eval      pq_chkwatc = pq_chkwatc - 1
     c                   eval      pq_chkfrez = pq_chkfrez + 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkwatc = so_chkwatc - 1
     c                   eval      so_chkfrez = so_chkfrez + 1
     c***********        if        pq_chkchg = 0
     c***********        eval      pq_chkchg = 1
     c***********        eval      so_chkchg = so_chkchg + 1
     c***********        endif
     c                   eval      pr_forchg = 'S'
     c                   eval      pr_forfrez = iso_end
     c                   endif

     c                   if        (pr_usrstat = 'P') and
     c                             (zz_usrstat = 'F')
     c                   eval      pq_chkprob = pq_chkprob - 1
     c                   eval      pq_chkfrez = pq_chkfrez + 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkprob = so_chkprob - 1
     c                   eval      so_chkfrez = so_chkfrez + 1
     c***********        if        pq_chkchg = 0
     c***********        eval      pq_chkchg = 1
     c***********        eval      so_chkchg = so_chkchg + 1
     c***********        endif
     c                   move      '0001-01-01'  pr_probdat
     c                   eval      pr_forfrez = iso_end
     c                   eval      pr_forchg = 'S'
     c                   endif

     c                   if        (pr_usrstat = 'M') and
     c                             (zz_usrstat = 'F')
     c                   eval      pq_chkmanl = pq_chkmanl - 1
     c                   eval      pq_chkfrez = pq_chkfrez + 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkmanl = so_chkmanl - 1
     c                   eval      so_chkfrez = so_chkfrez + 1
     c************       if        pq_chkchg = 0
     c************       eval      pq_chkchg = 1
     c************       eval      so_chkchg = so_chkchg + 1
     c************       endif
     c                   eval      pr_forchg = 'S'
     c                   eval      pr_forfrez = iso_end
     c                   endif

     c                   if        (pr_usrstat = 'F') and
     c                             (zz_usrstat = 'W')
     c                   eval      pq_chkwatc = pq_chkwatc + 1
     c                   eval      pq_chkfrez = pq_chkfrez - 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkwatc = so_chkwatc + 1
     c                   eval      so_chkfrez = so_chkfrez - 1
     c**********         if        pq_chkchg = 0
     c**********         eval      pq_chkchg = 1
     c**********         eval      so_chkchg = so_chkchg + 1
     c**********         endif
     c                   move      '0001-01-01'  pr_forfrez
     c                   eval      pr_forchg = 'S'
     c                   endif

     c                   if        (pr_usrstat = 'F') and
     c                             (zz_usrstat = 'M')
     c                   eval      pq_chkmanl = pq_chkmanl + 1
     c                   eval      pq_chkfrez = pq_chkfrez - 1
     c                   eval      pq_usrstat = zz_usrstat
     c                   eval      so_chkmanl = so_chkmanl + 1
     c                   eval      so_chkfrez = so_chkfrez - 1
     c**********         if        pq_chkchg = 0
     c**********         eval      pq_chkchg = 1
     c**********         eval      so_chkchg = so_chkchg + 1
     c**********         endif
     c                   move      '0001-01-01'  pr_forfrez
     c                   eval      pr_forchg = 'S'
     c                   endif


     c                   if        (pr_usrstat = ' ') and
     c                             (zz_usrstat = 'F')
     c                   eval      pq_chkfrez = pq_chkfrez + 1
     c                   eval      so_chkfrez = so_chkfrez + 1
     c                   eval      pq_usrstat = zz_usrstat
     c***********        if        pq_chkchg = 0
     c***********        eval      pq_chkchg = 1
     c***********        eval      so_chkchg = so_chkchg + 1
     c***********        endif
     c                   eval      pr_forfrez = iso_end
     c                   eval      pr_forchg = 'S'
     c                   endif

     c                   if        zz_usrstat <> *blanks and
     c                             zz_usrstat <> 'P'
     c                   eval      pr_usrstat = zz_usrstat
     c                   endif

     c                   update    rk_product
     c                   if        *in17 = *off
     c                   update    rk_prodsoq
     c                   endif
     c                   if        *in16 = *off
     c                   update    rk_suplsoq
     c                   endif

     c                   endif
     c                   endsr

      * /////////////////////////////////////////////// Write k_prodfor

     c     $_wrt_prod    begsr

     c                   call      'K3S_3041  '
     c                   parm                    comp
     c                   parm                    pf_locn
     c                   parm                    pf_supl
     c                   parm                    pf_suplsub
     c                   parm                    pf_prod
     c                   parm                    pf_birth
     c                   parm                    pf_birthtm
     c                   parm                    pf_chgtype
     c                   parm                    pf_chgdesc
     c                   parm                    pf_avgbef
     c                   parm                    pf_avgaft
     c                   parm                    pf_avgdiff
     c                   parm                    pf_seasbef
     c                   parm                    pf_seasaft
     c                   parm                    pf_devpbef
     c                   parm                    pf_devpaft
     c                   parm                    pf_statbef
     c                   parm                    pf_stataft
     c                   parm                    pf_sysstat
     c                   parm                    pf_user
     c                   parm                    pf_workstn
     c                   parm                    pf_program
     c                   endsr

      * ////////////////////////////////////////////////////// Edit end date

     c     $_edt_endz    begsr

      * edit requested end date
     c                   eval      errors    = *off
     c                   clear                   iso_end
      * call module to convert user entered date into *ISO format
     c                   call      'K3S_M130'
     c                   parm                    zz_frezend
     c                   parm                    lda_usrdat
     c                   parm                    lda_date6
     c                   parm                    iso_end
     c                   parm      0             iso_error

      *      requested end date entered, not valid
     c                   if        iso_error = 1
     c                   eval      errors    = *on
     c                   eval      *in29     = *on
     c                   eval      @msg_id   = 'K3_9040'
     c                   eval      @msg_text = zz_frezend
     c                   exsr      $_add_msg
     c                   else
     c                   eval      *in29 = *off
     c                   endif


     c                   endsr

      * /////////////////////////////////////////////// new history

     c     $_fix_hist    begsr
     c                   if        errors = *off
     c                   if        zz_toforci < 52
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks)
     c                   eval      *in64 = *off
     c     lc_keyx       setll     rk_locatns
     c                   read(n)   rk_locatns                             64
     c                   eval      *in64 = *off
     c     lc_keyy       setll     rk_locatns
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_tolocn = lc_locn
     c                   endif
     c                   endif
     c                   eval      *in02 = *off
     c     *hival        setll     rk_prodhis
     c     ph_toold      setll     rk_prodhis
     c     ph_toold      reade     rk_prodhis                             02

     c                   dow       *in02 = *off


     c                   delete    rk_prodhis
     c     ph_toold      reade     rk_prodhis                             02
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks) and
     c                             *in02 = *on
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_tolocn = lc_locn
     c                   eval      *in02 = *off
     c     *hival        setll     rk_prodhis
     c     ph_toold      setll     rk_prodhis
     c     ph_toold      reade     rk_prodhis                             02
     c                   endif
     c                   endif
     c                   enddo
     c                   endif
     c                   if        zz_toforci = 52
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks)
     c                   eval      *in64 = *off
     c     lc_keyx       setll     rk_locatns
     c                   read(n)   rk_locatns                             64
     c                   eval      *in64 = *off
     c     lc_keyy       setll     rk_locatns
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_tolocn = lc_locn
     c                   endif
     c                   endif
     c                   eval      *in02 = *off
     c     *hival        setll     rk_prodh52
     c     ph_toold      setll     rk_prodh52
     c     ph_toold      reade     rk_prodh52                             02

     c                   dow       *in02 = *off


     c                   delete    rk_prodh52
     c     ph_toold      reade     rk_prodh52                             02
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks) and
     c                             *in02 = *on
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_tolocn = lc_locn
     c                   eval      *in02 = *off
     c     *hival        setll     rk_prodh52
     c     ph_toold      setll     rk_prodh52
     c     ph_toold      reade     rk_prodh52                             02
     c                   endif
     c                   endif
     c                   enddo
     c                   endif

     c                   if        zz_frforci < 52
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks)
     c                   eval      *in64 = *off
     c     lc_keyx       setll     rk_locatns
     c                   read(n)   rk_locatns                             64
     c                   eval      *in64 = *off
     c     lc_keyy       setll     rk_locatns
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_tolocn = lc_locn
     c                   eval      zz_frlocn = lc_locn
     c                   endif
     c                   endif
     c                   eval      *in02 = *off
     c     *hival        setll     rk_prodhis
     c     ph_frnew      setll     rk_prodhis
     c     ph_frnew      reade(n)  rk_prodhis                             02

     c                   dow       *in02 = *off

     c                   eval      ph_supl    = zz_tosupl
     c                   eval      ph_suplsub = zz_tosub
     c                   eval      ph_suplusr = pr_suplusr
     c                   eval      ph_suplusb = pr_suplusb
     c                   eval      ph_prod    = zz_toprod
     c     ph_tonew      chain     rk_prodhis                         62
     c                   if        *in62 = *on
     c                   write     rk_prodhis
     c                   endif
     c     pr_tokey      chain(n)  rk_product                         62
     c                   if        *in62 = *off
     c     pr_tokey      chain     rk_product                         62
     c                   eval      pr_forcint = zz_frforci
     c                   eval      pr_forcyr  = xx_forcyr
     c                   eval      pr_forcper = xx_forcper
     c                   update    rk_product
     c                   endif
     c     ph_frnew      reade(n)  rk_prodhis                             02
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks) and
     c                             *in02 = *on
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_tolocn = lc_locn
     c                   eval      zz_frlocn = lc_locn
     c                   eval      *in02 = *off
     c     *hival        setll     rk_prodhis
     c     ph_frnew      setll     rk_prodhis
     c     ph_frnew      reade(n)  rk_prodhis                             02
     c                   endif
     c                   endif
     c                   enddo
     c                   endif

     c                   if        zz_frforci = 52
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks)
     c                   eval      *in64 = *off
     c     lc_keyx       setll     rk_locatns
     c                   read(n)   rk_locatns                             64
     c                   eval      *in64 = *off
     c     lc_keyy       setll     rk_locatns
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_tolocn = lc_locn
     c                   eval      zz_frlocn = lc_locn
     c                   endif
     c                   endif
     c                   eval      *in02 = *off
     c     *hival        setll     rk_prodh52
     c     pw_frnew      setll     rk_prodh52
     c     pw_frnew      reade(n)  rk_prodh52                             02

     c                   dow       *in02 = *off

     c                   eval      pw_supl    = zz_tosupl
     c                   eval      pw_suplsub = zz_tosub
     c                   eval      pw_suplusr = pr_suplusr
     c                   eval      pw_suplusb = pr_suplusb
     c                   eval      pw_prod    = zz_toprod
     c     pw_tonew      chain     rk_prodh52                         62
     c                   if        *in62 = *on
     c                   write     rk_prodh52
     c                   endif
     c     pr_tokey      chain(n)  rk_product                         62
     c                   if        *in62 = *off
     c     pr_tokey      chain     rk_product                         62
     c                   eval      pr_forcint = zz_frforci
     c                   eval      pr_forcyr  = xx_forcyr
     c                   eval      pr_forcper = xx_forcper
     c                   update    rk_product
     c                   endif
     c     pw_frnew      reade(n)  rk_prodh52                             02
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks) and
     c                             *in02 = *on
     c     lc_keyy       reade(n)  rk_locatns                             64
     c                   if        *in64 = *off
     c                   eval      zz_tolocn = lc_locn
     c                   eval      zz_frlocn = lc_locn
     c                   eval      *in02 = *off
     c     *hival        setll     rk_prodh52
     c     pw_frnew      setll     rk_prodh52
     c     pw_frnew      reade(n)  rk_prodh52                             02
     c                   endif
     c                   endif
     c                   enddo
     c                   endif

     c                   endif
     c                   if        (sz_frlocn = *blanks) and
     c                             (sz_tolocn = *blanks)
     c                   eval      zz_tolocn = sz_tolocn
     c                   eval      zz_frlocn = sz_frlocn
     c                   endif
     c                   endsr

      * ////////////////////////////////////////////////////////////////////


      * ---------------------------------- Message logic for screen programs
     c/copy k3s_c051

      * -----------------------------------------Cursor posistion subroutine
     c/copy k3s_c080

      * -----------------------------------------$_get_lda  subroutine
     c/copy k3s_c031

      * -----------------------------------------$_get_time subroutine
     c/copy k3s_c180

      * -----------------------------------------------$_usr_exit subroutine
     c/copy k3s_c190

      * -----------------------------------------------$_f08_exit subroutine
     c/copy k3s_c191

      * -----------------------------------------------$_get_titl subroutine
     c/copy k3s_c200

