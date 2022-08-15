      *****************************************************************
     h copyright('(C) Copyright 1996 - 2010 King III Solutions, Inc.  +
     h Rel 5.05 2010-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2010 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_2067
      **   Type: ILE RPG Program
      **   Desc: Supplier discount bracket change log review - display log
      **
      *****************************************************************
      **
      **  Indicator usage
      **
      **      record formats
      **  11  xk_dealsum
      **  12  rk_suplier
      **  13  xk_suplier
      **  14  rk_locatns
      **  15  rk_dealsum
      **  16  rk_buyrgrp
      **  17  K3_2067_1s
      **
      *****************************************************************

     fK3S_2067fmcf   e             workstn infds(infds)                         work station file
     f                                     sfile(K3_2067_1s:sfile_rrn1)         deals by buy group
     f                                     sfile(K3_2067_fs:sfile_rrn1)         deals by buy group

     fk_supliercif   e           k disk                                         suppliers
      * suppliers by supplier, sub supplier

     fk_locatnsaif   e           k disk                                         locations
      * locations by location

     fk_supdlogauf a e           k disk                                         deal summary info
      * selected products

     fk_tablcodaif   e           k disk                                         table codes
      * table file

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

      * -------------------------------------- Field names for QCLSCAN logic
     d/copy k3s_c070

      * ------------------------------------ D-specs for common workfields
     d/copy k3s_c270

     d zz_codetyp      s                   like(ta_codetyp)                     location
     d zz_codeval      s                   like(ta_codeval)                     location
     d blank           s                   like(lc_comp)                        location
     d screen1of       s                   like(lc_comp)                        location

      * --------------------------------------------------------- Workfields
     d save_locn       s                   like(lc_locn)                        save location id
     d save_supl       s                   like(sp_supl)                        save supplier id
     d save_sub        s                   like(sp_suplsub)                     save sub supplier id
     d save_chgt       s                   like(d0_chgtype)                     save buy group id
     d save_begin      s                   like(zz_begin)                       save batch beg date
     d save_end        s                   like(zz_end)                         save batch end date
     d xx_mode         s                   like(mode)                           save batch end date
     d xx_prodseq      s                   like(prodseq)                        save batch end date
     d xx_soqseq#      s                   like(soqseq#)                        save batch end date
     d test3_flag      s                   like(ta_flag3)                       save batch end date

      * ------------------------------- Named indicators for file operations
     d locatnsNOT      c                   14                                   Error date entered
     d tablspcNOT      c                   17                                   Error date entered
     d suplierNOT      c                   13                                   Error date entered
     d buyrgrpNOT      c                   16                                   Error date entered

      * -------------------------------------------------- Parameters passed

      * parameters passed to program
     c     *entry        plist                                                  parameters passed
     c                   parm                    locn                           location id
     c                   parm                    supl                           supplier id
     c                   parm                    suplsub                        sub supplier id
     c                   parm                    screen1of                      sub supplier id
     c                   parm                    @returned                      product search value

      * ---------------------------------------------------------- Key Lists

      * key list for supplier file by supplier
     c     sp_key        klist                                                  key list suppliers
     c                   kfld                    sp_comp                        company
     c                   kfld                    sp_supl                        supplier

      * key list for locations file
     c     lc_key        klist                                                  key list locations
     c                   kfld                    lc_comp                        company
     c                   kfld                    lc_locn                        location

      * key list for selected products review - detail products
     c     d0_key        klist                                                  key list sel batches
     c                   kfld                    lda_comp                       company
     c                   kfld                    zz_locn                        code type
     c                   kfld                    zz_supl                        code type
     c                   kfld                    zz_suplsub                     code type

      * key list for table file
     c     ta_key        klist                                                  key list table
     c                   kfld                    ta_comp                        company
     c                   kfld                    ta_codetyp                     code type
     c                   kfld                    ta_codeval                     code value

      * ------------------------------------------------------- Once Routine
      * once routine
     c                   exsr      $_once

      * ----------------------------------------------- display batches loop

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
     c                   endif

      * get time formatted
     c                   exsr      $_get_time

      * if errors exist, on select fields write message and call window
     c                   if        (*in(locatnsNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (*in(suplierNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (*in(tablspcNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (*in(buyrgrpNOT) = *on) and
     c                             (lda_window = 1)
     c                   exsr      $_cur_sel
     c                   eval      *in(suplierNOT) = *off
     c                   eval      *in(locatnsNOT) = *off
     c                   eval      *in(buyrgrpNOT) = *off
     c                   eval      *in(tablspcNOT) = *off
     c                   else
      * -------------------------------------------- batches display screens
     c                   write     k3_ctl_msg
     c                   write     K3_2067_1r

      * ------------------------------------------------------ start 21 mode
     c                   if        *in21 = *on

      * prime F8=User exit text
     c                   exsr      $_usr_exit

      * get screen title
     c                   eval      @recd_frmt = '1C'
     c                   exsr      $_get_titl

     c                   exfmt     K3_2067_1c
     c                   if        key_press = f21_key
     c                   eval      *in22 = *on
     c                   eval      *in21 = *off
     c                   eval      screenon = '21'
     c                   endif
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

     c                   endif
      * -------------------------------------------------------- end 21 mode

      * ------------------------------------------------------ start 22 mode
     c                   if        *in22 = *on

      * prime F8=User exit text
     c                   exsr      $_usr_exit

      * get screen title
     c                   eval      @recd_frmt = '1C'
     c                   exsr      $_get_titl

     c                   write     k3_ctl_msg
     c                   write     K3_2067_1r
     c                   exfmt     K3_2067_fc
     c                   if        key_press = f19_key and
     c                             screenon = '21'
     c                   eval      *in21 = *on
     c                   eval      *in22 = *off
     c                   endif
     c                   if        key_press = f19_key
     c                   eval      screenon = '  '
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

     c                   endif
      * -------------------------------------------------------- end 22 mode

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
     c                   eval      send_error = *off

      * position cursor control
     c                   eval      *in51 = *off
     c                   eval      *in53 = *off
     c                   eval      *in54 = *off
     c                   eval      *in55 = *off
     c                   eval      *in56 = *off
     c                   eval      *in57 = *off
     c                   eval      *in58 = *off
     c                   eval      *in59 = *on

      * re-display after valid command key list
     c                   eval      re_display = *off

      * display valid comand keys
     c                   if        key_press = f24_key
      * get time formatted
     c                   exsr      $_get_time
     c                   exfmt     K3_2067_1f
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
     c                   parm                    zz_program                     location id
     c                   parm      *zeros        @returned                      location id
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

      * user has not selected to exit
     c                   if        (key_press <> f03_key) and
     c                             (finished = *off)

     c                   select
      * user wants to roll up (page down) through the subfile
     c                   when      key_press = rollup_key and
     c                             zz_birth  <> @no_recs
     c                   if        save_botom = *zeros
     c                   exsr      $_bld_subf
     c                   else
     c     save_botom    div       12            sv_botom
     c                   if        sv_dec = *zeros
     c                   eval      zr_dsprrn1 = save_botom -11
     c                   eval      sfile_rrn1 = save_botom -11
     c                   else
     c     sv_num        mult      12            zr_dsprrn1
     c                   eval      zr_dsprrn1 = zr_dsprrn1 + 1
     c                   eval      sfile_rrn1 = zr_dsprrn1
     c                   endif
     c                   endif

      * user wants to roll down (page up) through the subfile
     c                   when      key_press = rolldn_key
     c                   eval      sfile_rrn1 = sfile_rrn1 - 12
     c                   if        sfile_rrn1 >= 0
     c                   eval      zr_dsprrn1 = 1
     c                   eval      sfile_rrn1 = 1
     c                   endif

      * F01 HELP exit
     c                   when      key_press = f01_key
     c/COPY K3S_C192

      * f08 user exit
     c                   when      key_press = f08_key
     c                   eval      @8_locn = locn
     c                   eval      @8_buyr = *blanks
     c                   eval      @8_supl = supl
     c                   eval      @8_suplsub = suplsub
     c                   eval      @8_prod = *blanks
     c                   exsr      $_f8_exit


      ****************************
      ****************************
      * user wants to change selection of products being displayed
     c                   when      (zz_locn    <> save_locn)   or
     c                             (zz_supl    <> save_supl)   or
     c                             (zz_suplsub <> save_sub)    or
     c                             (zz_chgtype <> save_chgt)   or
     c                             (zz_begin   <> save_begin)  or
     c                             (zz_end     <> save_end)

     c                   eval      chg_file = *on
     c                   eval      save_top = *zeros
     c                   eval      save_botom = *zeros
     c                   eval      save_rrn3 = *zeros
     c                   eval      save_rrn5 = *zeros
      *     edit location entry
     c                   exsr      $_edt_locn
      *     edit supplier entry
     c                   if        errors = *off
     c                   exsr      $_edt_supl
     c                   endif
      *     edit review begin date entry
     c                   if        errors = *off
     c                   exsr      $_edt_beg
     c                   endif
      *     edit review end date entry
     c                   if        errors = *off
     c                   exsr      $_edt_end
     c                   endif
      *     edit change type
     c                   if        errors = *off
     c                   exsr      $_edt_chgt
     c                   endif

      * user wants to toggle display
     c                   when      key_press = f20_key
     c                   exsr      $_edt_togl

      * user wants to toggle display
     c                   when      key_press = f22_key
     c                   exsr      $_edt_togl

      ****************************
      * edit subfile options

     c                   endsl

      * if errors exist, pass error message
     c                   if        errors = *on
     c                   exsr      $_pass_msg
     c                   else

      * if user did not select to roll up (page down), then the user has
      * either changed the components to display a new page, or selected
      * a specific product to process. re-create page
     c                   if        (key_press <> rollup_key) and
     c                             (key_press <> rolldn_key) and
     c                             (zz_process<> *on) and
     c                             (key_press <> f05_key) and
     c                             (key_press <> f06_key) and
     c                             (key_press <> f07_key) and
     c                             (key_press <> f08_key) and
     c                             (key_press <> f20_key) and
     c                             (key_press <> f22_key) and
     c                             (errors = *off)

     c                   if        chg_file = *on
      * clear subfile of product records
     c                   eval      *in20 = *on
     c                   write     K3_2067_1c
     c                   write     K3_2067_fc
     c                   eval      *in20 = *off

      * initialize rrn values
     c                   eval      save_top = *zeros
     c                   eval      save_botom = *zeros
     c                   eval      save_rrn3 = *zeros
     c                   eval      save_rrn5 = *zeros
     c                   clear                   save_rrn1
     c                   clear                   sfile_rrn1
     c                   clear                   zr_dsprrn1

      * re-position cursor on screen to change type
     c                   eval      *in59 = *on

      * re-position file cursor
     c                   eval      lda_comp    = lda_comp
     c     d0_key        setll     rk_supdlog

      * rebuild subfile of selected products batches
     c                   eval      save_locn  = zz_locn
     c                   eval      save_supl  = zz_supl
     c                   eval      save_chgt  = zz_chgtype
     c                   eval      save_sub   = zz_suplsub
     c                   eval      save_begin = zz_begin
     c                   eval      save_end   = zz_end
     c                   exsr      $_bld_subf
     c                   endif
     c                   endif
     c*                  if        (key_press = enter_key) and
     c                   if        (chg_file = *off) and
     c                             (key_press <> rollup_key) and
     c                             (key_press <> rolldn_key) and
     c                             (key_press <> f20_key) and
     c                             (key_press <> f22_key) and
     c                             (errors = *off) and
     c                             (zz_process = *off)
     c                   if        zz_csrrrn > *zeros
     c                   eval      zr_dsprrn1 = zz_csrrrn
     c                   eval      sfile_rrn1 = zz_csrrrn
     c                   else
     c                   eval      zr_dsprrn1 = 1
     c                   eval      sfile_rrn1 = 1
     c                   endif
     c                   endif
     c                   eval      zz_process = *off
     c                   eval      chg_file = *off

      * end of roll up test
     c                   endif

      * end of user selections
     c                   endif

      * end of main loop
     c/copy k3s_c250
     c                   enddo

      * --------------------------------------------------- End of Main Loop

      * finished, set on LR
     c                   if        @returned = 12
     c                   eval      @returned = *zeros
     c                   endif

     c                   eval      *inlr = *on

      * ***************************************************** End of program

      * /////////////////////////////////////////////////////// Once routine

     c     $_once        begsr

      * retrieve data area *lda, and format information
     c                   exsr      $_get_lda

      * prime program message queue
     c                   eval      msgpgq = '*'

      * prime program id
     c                   eval      zz_program = psds_progm

      * prime company code
     c                   eval      zz_compcod = lda_compcd

      * prime user id
     c                   eval      zz_user = psds_user


      * prime location id
     c                   eval      zz_locn   = locn
     c                   eval      save_locn = locn

      * prime location id
     c                   eval      zz_chgtype  = *blanks
     c                   eval      save_chgt = *blanks

      * prime supplier id
     c                   eval      zz_supl   = supl
     c                   eval      save_supl = supl

      * prime sub supplier id
     c                   eval      zz_suplsub = suplsub
     c                   eval      save_sub   = suplsub

      * prime review end date
      * call module to convert date to user specification
     c                   call      'K3S_M100'
     c                   parm                    lda_cmpdat
     c                   parm                    lda_usrdat
     c                   parm                    lda_date6
     c                   parm                    char_date

     c                   eval      zz_end = %triml(char_date)
     c                   eval      save_end = zz_end

      *    initialize for first put
     c                   move      lda_cmpdat    iso_end

      * prime review begin date
     c                   eval      zz_begin   = *blanks
     c                   eval      save_begin = *blanks

      * prime processing mode
     c*                  eval      zz_modedsp = @mod_view

      * set finished indicator to off
     c                   eval      finished = *off

      * set error off
     c                   eval      errors = *off

      * position cursor to change type
     c                   eval      *in59 = *on

      * position cursor to 1st subfile record
     c                   eval      *in21 = *off
     c                   eval      *in22 = *off
     c                   if        screen1of = '1'
     c                   eval      *in21 = *on
     c                   endif
     c                   if        screen1of = '2'
     c                   eval      *in22 = *on
     c                   endif

      * get user update authority flag
      * call module to obtain user update authority flag
     c                   call      'K3S_9050'
     c                   parm                    zz_program
     c                   parm      0             @usr_updat
     c                   eval      *in94 =      (@usr_updat = 0)

      * prime F8=User exit text
     c                   exsr      $_usr_exit

      * get screen title once
     c                   eval      @recd_frmt = '1C'
     c                   exsr      $_get_titl


      *  Check if Replenish user
     c                   eval      ta_comp    = lda_comp
     c                   eval      ta_codetyp = 'USR'
     c                   eval      ta_codeval = psds_user
     c     ta_key        chain     rk_tablcod                         90

     c                   exsr      $_edt_supl

      * prime buy group id
      * start with first batch record
     c     d0_key        setll     rk_supdlog

      * build subfile of batches
     c                   exsr      $_bld_subf

     c                   endsr

      * /////////////////////////////////////////// Build subfile of batches

     c     $_bld_subf    begsr
     c*                  if        save_top = *zeros
     c*                  write     k3_2067_sr
     c*                  endif

      * used for end of file read
     c                   eval      *in11 = *off

      * current size
     c                   if        save_rrn1 < save_rrn3
     c     save_rrn3     chain     k3_2067_1s                         76
     c     d0_key        setll     rk_supdlog
     c                   read(n)   rk_supdlog                             11
     c                   eval      sfile_rrn1 = save_rrn3
     c                   eval      save_rrn3 = *zeros
     c                   else
     c                   eval      sfile_rrn1 = save_rrn1
     c                   eval      save_rrn3 = *zeros
     c                   endif

      * initialize fields
     c                   clear                   line_count

      * read product record
     c                   read(n)   rk_supdlog                             11

      * read through product records until end of file, or one page filled,
      *      or until new batch product read
     c*                  dow       *in11 = *off
     c                   dow       (*in11 = *off) and
     c                             (line_count < 12)

      * initialize fields

      * if buy group id selected is blank, take all buy groups           )
      * if user has selected a specific buy group id, select it only
     c                   if        (lda_comp = *blanks)         or

     c                             ((lda_comp <> *blanks) and
     c                              (lda_comp =  d0_comp))


      * if location id selected is blank, take all locations         (or)
      * if user has selected a specific location id, select it only

     c                   if        (zz_locn = *blanks)         or

     c                             ((zz_locn <> *blanks) and
     c                              (zz_locn =  d0_locn))

      * if supplier id selected is blank, take all suppliers         (or)
      * if user has selected a specific supplier id, select it only

     c                   if        ((zz_supl    = *blanks) and
     c                              (zz_suplsub = *blanks))         or

     c                             (((zz_supl    <> *blanks) or
     c                               (zz_suplsub <> *blanks))       and
     c                              ((zz_supl    = d0_supl) and
     c                               (zz_suplsub = d0_suplsub)))

      * if review begin date selected is blank, take all batches       (or)
      * if user has entered a specific review begin date, select only those
      *         batches that begin on or after the date entered

     c                   if        (zz_begin = *blanks)         or

     c                             ((zz_begin <> *blanks) and
     c                             (d0_birth >= iso_begin))

      * if review end date selected is blank, take all batches       (or)
      * if user has entered a specific review end date, select only those
      *         batches that end on or before the date entered

     c                   if        (zz_end = *blanks)         or

     c                             ((zz_end   <> *blanks) and
     c                             (d0_birth <= iso_end))


     c                   if        (zz_chgtype = *blanks)         or

     c                             ((zz_chgtype <> *blanks) and
     c                              (zz_chgtype =  d0_chgtype))

      * if review begin date selected is blank, take all batches       (or)
      * build subfile record with selected products review information
     c                   exsr      $_bld_recd

      * keep track of rrn
     c                   eval      sfile_rrn1 = sfile_rrn1 + 1

      * keep track of line count
     c                   eval      line_count = line_count + 1

      * set redisplay at first subfile record selected
     c                   if        line_count = 1
     c                   eval      zr_dsprrn1 = sfile_rrn1
     c                   endif

      * remember rrn
     c                   eval      save_rrn1  = sfile_rrn1
     c                   eval      save_rrn3  = sfile_rrn1
     c                   eval      save_rrn5  = sfile_rrn1

      * write to subfile
     c                   write     K3_2067_1s
     c                   write     K3_2067_fs

     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif

      * read the next product record, if page not complete
     c                   if        line_count <> 12
     c                   read(n)   rk_supdlog                             11

      * stay within this batch of products. if this record is not in batch,
      *    then exit early from routine, and set on indicator 11
      *    to specify END.... for user.
     c                   if        (d0_comp  <> lda_comp)
     c                   eval      *in11     = *on
     c                   endif

     c                   if        ((zz_locn <> *blanks) and
     c                              (zz_locn <>  d0_locn))
     c                   eval      *in11     = *on
     c                   endif

     c                   if        ((zz_supl <> *blanks) and
     c                              (zz_supl <>  d0_supl))
     c                   eval      *in11     = *on
     c                   endif

     c                   if        ((zz_suplsub <> *blanks) and
     c                              (zz_suplsub <>  d0_suplsub))
     c                   eval      *in11     = *on
     c                   endif

     c                   endif

     c                   enddo
     c                   if        (*in11 = *on) and
     c                             (sfile_rrn1 <> 0)
     c                   eval      save_botom = sfile_rrn1
     c                   endif

      * if no records exist for this combination, provide message
     c                   if        sfile_rrn1 = 0
     c                   eval      sfile_rrn1 = sfile_rrn1 + 1
     c                   eval      zr_dsprrn1 = sfile_rrn1
     c                   exsr      $_clr_dsp
     c                   eval      zz_birth   = @no_recs

     c                   eval      *in51 = *on
     c                   write     K3_2067_1s
     c                   write     K3_2067_fs
     c                   eval      *in51 = *off

      * position cursor to supplier
     c                   eval      *in51 = *on

     c                   endif

     c                   exsr      $_get_time
     c                   endsr

      * /////////////////////////////////////////////// build subfile record

     c     $_bld_recd    begsr
     c                   if        save_top = *zeros
     c                   eval      save_top = 1
     c                   endif
      * call module to convert date to user specification
     c                   call      'K3S_M100'
     c                   parm                    d0_birth
     c                   parm                    lda_usrdat
     c                   parm                    not_six
     c                   parm                    char_date

     c                   eval      zz_birth = %triml(char_date)

     c                   call      'K3S_M110'
     c                   parm                    d0_birthtm
     c                   parm                    lda_usrtim
     c                   parm                    zz_time

      *  Check if Replenish user
     c                   eval      ta_comp    = lda_comp
     c                   eval      ta_codetyp = 'USR'
     c                   eval      ta_codeval = d0_user
     c     ta_key        chain     rk_tablcod                         88
     c                   if        *in88 = *off
     c                   movel     ta_codeds1    zz_usernam
     c                   else
     c                   eval      zz_usernam = '* Not in K3S * '
     c                   endif

     c                   endsr

      * ////////////////////////////////////////////////////// Edit supplier

     c     $_edt_supl    begsr

      * set read indicator off
     c                   eval      *in(suplierNOT) = *off

      * prime key list for supplier
     c                   eval      sp_comp    = lda_comp
     c                   eval      sp_supl    = zz_supl

      * blank supplier would mean all suppliers
     c                   if        zz_supl   = *blanks
     c                   eval      save_supl = zz_supl
     c                   eval      sp_name = 'All suppliers'

      * otherwise, validate entry
     c                   else

      * get supplier record
     c     sp_key        setll     rk_suplier
     c     sp_key        reade     rk_suplier                             13

      *     supplier record not found, send back error
     c                   if        *in(suplierNOT)
     c                   eval      errors    = *on
     c                   eval      send_error = *on
     c                   eval      *in54     = *on
     c                   eval      @msg_id   = 'K3_2000'
     c                   eval      @msg_text = zz_supl
     c                   exsr      $_add_msg
     c                   endif

     c                   endif

     c                   endsr

      * ///////////////////////////////////////////// Clear sub file display

     c     $_clr_dsp     begsr

      * clear sub file display

     c                   clear                   d0_chgtype
     c                   clear                   d0_discbkt
     c                   clear                   d0_dis1typ
     c                   clear                   d0_dis1val
     c                   clear                   d0_dis1unt
     c                   clear                   d0_dis2typ
     c                   clear                   d0_dis2val
     c                   clear                   d0_dis2unt
     c                   clear                   d0_disrate
     c                   clear                   d0_dissavg
     c                   clear                   zz_birth
     c                   clear                   zz_usernam

     c                   endsr

      * //////////////////////////////////////////////// Edit location entry

     c     $_edt_locn    begsr

      * prime key list to read this company only
     c                   eval      lc_comp    = lda_comp
     c                   eval      lc_locn    = zz_locn


      * get location record
     c     lc_key        chain     rk_locatns                         14

      *      location does not exist
     c                   if        *in(locatnsNOT)
     c                   eval      errors    = *on
     c                   eval      send_error= *on
     c                   eval      *in53     = *on
     c                   eval      @msg_id   = 'K3_8020'
     c                   eval      @msg_text = zz_locn
     c                   exsr      $_add_msg

     c                   endif


     c                   endsr

      * //////////////////////////////////////////////////// Edit begin date

     c     $_edt_beg     begsr

      * blank requested begin date would not exclude any batches
     c                   if        zz_begin   = *blanks
     c                   eval      save_begin = zz_begin

      * otherwise, validate entry
     c                   else

      * edit requested begin date
     c                   clear                   iso_begin
      * call module to convert user entered date into *ISO format
     c                   call      'K3S_M130'
     c                   parm                    zz_begin
     c                   parm                    lda_usrdat
     c                   parm                    lda_date6
     c                   parm                    iso_begin
     c                   parm      0             iso_error

      *      requested begin date entered, not valid
     c                   if        iso_error = 1
     c                   eval      errors    = *on
     c                   eval      *in57     = *on
     c                   eval      @msg_id   = 'K3_9030'
     c                   eval      @msg_text = zz_begin
     c                   exsr      $_add_msg
     c                   else
     c                   eval      save_begin = zz_begin
     c                   endif

     c                   endif

     c                   endsr

      * ////////////////////////////////////////////////////// Edit end date

     c     $_edt_end     begsr

      * blank requested end date would not exclude any batches
     c                   if        zz_end   = *blanks
     c                   eval      save_end = zz_end

      * otherwise, validate entry
     c                   else

      * edit requested end date
     c                   clear                   iso_end
      * call module to convert user entered date into *ISO format
     c                   call      'K3S_M130'
     c                   parm                    zz_end
     c                   parm                    lda_usrdat
     c                   parm                    lda_date6
     c                   parm                    iso_end
     c                   parm      0             iso_error

      *      requested end date entered, not valid
     c                   if        iso_error = 1
     c                   eval      errors    = *on
     c                   eval      *in58     = *on
     c                   eval      @msg_id   = 'K3_9040'
     c                   eval      @msg_text = zz_end
     c                   exsr      $_add_msg
     c                   else
     c                   eval      save_end = zz_end
     c                   endif

      *   the next two tests would take place only if the user has keyed
      *       in both a begin and end date, which passed previous edits
     c                   if        (zz_begin <> *blanks) and
     c                             (zz_end   <> *blanks) and
     c                             (errors    = *off)

      *      requested begin date should not be greater than requested end
     c                   if        (iso_begin > iso_end)
     c                   eval      errors    = *on
     c                   eval      *in57     = *on
     c                   eval      @msg_id   = 'K3_9050'
     c                   eval      @msg_text = *blanks
     c                   exsr      $_add_msg
     c                   else
     c                   eval      save_end = zz_end
     c                   eval      save_begin = zz_begin
     c                   endif

     c                   endif

     c                   endif

     c                   endsr

      * //////////////////////////////////////Call appropriate selection Pgm

     c     $_cur_sel     begsr
     c                   if        (*in(locatnsNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_LOCN   ')

      * call module to obtain location numbers and names.
     c                   call      'K3S_9510'
     c                   parm                    lda_comp
     c                   parm                    zz_locn
     c                   parm                    send_error
     c                   parm      ' '           blank
     c                   endif


     c                   if        (*in(suplierNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_SUPL   ') or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_SUPLSUB')

      * call module to obtain supplier numbers and names.
     c                   call      'K3S_9520'
     c                   parm                    lda_comp
     c                   parm                    lda_buyr
     c                   parm                    zz_locn
     c                   parm                    zz_supl
     c                   parm                    zz_suplsub
     c                   parm                    send_error
     c                   parm      '1'           blank
     c                   endif



     c                   if        (*in(tablspcNOT) = *on) and
     c                             (lda_window = 1) or
     c                             (key_press = f04_key) and
     c                             (zz_field = 'ZZ_CHGTYPE')
     c                   eval      zz_codeval = %trimr(zz_chgtype)


      * call module to obtain table code file information for region
     c                   call      'K3S_9540'
     c                   parm                    lda_comp
     c                   parm      'SPC'         zz_codetyp
     c                   parm                    zz_codeval
     c                   parm                    send_error
     c                   parm      '1'           blank
     c                   parm      0             test3_flag
     c                   eval      zz_chgtype  = %trimr(zz_codeval)
     c                   endif



     c                   endsr
      * ////////////////////////////////////////////////// Edit change type

     c     $_edt_chgt    begsr

      * prime key list to read this company only
     c                   eval      ta_comp    = lda_comp
     c                   eval      ta_codetyp = 'SPC'
     c                   eval      ta_codeval = zz_chgtype
      * blank change type would mean all types
     c                   if        zz_chgtype   = *blanks
     c                   eval      save_chgt = zz_chgtype

      * otherwise, validate entry
     c                   else



      * get region record in table file
     c     ta_key        chain     rk_tablcod                         17

      *      change type does not exist
     c                   if        *in(tablspcNOT)
     c                   eval      errors    = *on
     c                   eval      send_error= *on
     c                   eval      *in59     = *on
     c                   eval      @msg_id   = 'K3_9020'
     c                   eval      @msg_text = zz_chgtype
     c                   exsr      $_add_msg

     c                   endif

     c                   endif

     c                   endsr

      * //////////////////////////////////////////////// Edit subfile toggle

     c     $_edt_togl    begsr


     c                   if        key_press = f20_key
     c                   if        (*in21 = *on)

     c                   eval      *in22 = *on
     c                   eval      *in21 = *off
     c                   else

     c                   if        (*in22 = *on)

     c                   eval      *in21 = *on
     c                   eval      *in22 = *off
     c                   endif
     c                   endif
     c                   endif

     c                   if        key_press = f22_key
     c                   if        (*in21 = *on)

     c                   eval      *in22 = *on
     c                   eval      *in21 = *off
     c                   else

     c                   if        (*in22 = *on)

     c                   eval      *in21 = *on
     c                   eval      *in22 = *off
     c                   endif
     c                   endif
     c                   endif

     c                   endsr

      * ////////////////////////////////////////////////////////////////////

      * ---------------------------------- Message logic for screen programs
     c/copy k3s_c051

      * -----------------------------------------Cursor posistion subroutine
     c/copy k3s_c080

      * ---------------------------------- gget lda subroutine
     c/copy k3s_c031

      * -----------------------------------------get time subroutine
     c/copy k3s_c180

      * -----------------------------------------------$_usr_exit subroutine
     c/copy k3s_c190

      * -----------------------------------------------$_f08_exit subroutine
     c/copy k3s_c191

      * -----------------------------------------------$_get_titl subroutine
     c/copy k3s_c200

