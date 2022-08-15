**************************************************************
     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')


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
      **   Name: K3S_1050AU
      **   Type: ILE RPG Program
      **   Desc: Suggested orders supplier automatic approval NITEJOB
      **
      *****************************************************************
      **  Indicator usage
      **
      **      record formats
      **
      *****************************************************************


     fK3S_1050fmcf   e             workstn indds(inds)                          work station file
     F                                     infds(infds)
     f                                     usropn                               work station file

     fk_companyauf   e           k disk                                         buy groups
      * Company values by company id

     fk_trancenaif   e           k disk                                         buy groups
      * Transfer supplier file

     fk_suplierauf   e           k disk                                         suppliers
      * suppliers by location, supplier, sub supplier

     fk_notepadauf a e           k disk                                         suppliers
      * note pad file

     fk_suplapvauf a e           k disk                                         suppliers
      * note pad file

     fk_locatnsaif   e           k disk                                         locations
      * locations by location

     fk_buyrgrpaif   e           k disk                                         buy groups
      * buy group by buy group

     fk_buyrsotauf   e           k disk                                         buy groups
      * buy group suggested order totals


     fk_suplsoqauf a e           k disk                                         deal summary info
      * selected products batches

     fk_suplpurbuf a e           k disk                                         deal summary info
      * supplier purchase history

     f*k_suplpodaif   e           k disk                                        deal summary info
      * supplier purchase history

     fk_tablcodaif   e           k disk                                         table codes
      * table file

      * ------------------------------------ File information data structure
     d*copy k3s_c010
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


      * ------------------------------------------- Function key definitions
     d*copy k3s_c020
      *****************************************************************
      **
      **   K3S-Replenish - Inventory REPLENISHment System
      **   Copyright (c) 1996-1997 by King III Solutions, Inc.
      **   All rights reserved.
      **
      *****************************************************************
      **
      **   Name: K3S_C020
      **   Type: ILE /COPY member
      **   Desc: Named constraints for function keys
      **
      *****************************************************************

     d f01_key         c                   const(X'31')                         function key 01
     d f02_key         c                   const(X'32')                         function key 02
     d f03_key         c                   const(X'33')                         function key 03
     d f04_key         c                   const(X'34')                         function key 04
     d f05_key         c                   const(X'35')                         function key 05
     d f06_key         c                   const(X'36')                         function key 06
     d f07_key         c                   const(X'37')                         function key 07
     d f08_key         c                   const(X'38')                         function key 08
     d f09_key         c                   const(X'39')                         function key 09
     d f10_key         c                   const(X'3A')                         function key 10
     d f11_key         c                   const(X'3B')                         function key 11
     d f12_key         c                   const(X'3C')                         function key 12
     d f13_key         c                   const(X'B1')                         function key 13
     d f14_key         c                   const(X'B2')                         function key 14
     d f15_key         c                   const(X'B3')                         function key 15
     d f16_key         c                   const(X'B4')                         function key 16
     d f17_key         c                   const(X'B5')                         function key 17
     d f18_key         c                   const(X'B6')                         function key 18
     d f19_key         c                   const(X'B7')                         function key 19
     d f20_key         c                   const(X'B8')                         function key 20
     d f21_key         c                   const(X'B9')                         function key 21
     d f22_key         c                   const(X'BA')                         function key 22
     d f23_key         c                   const(X'BB')                         function key 23
     d f24_key         c                   const(X'BC')                         function key 24
     d selct_pen       c                   const(X'3F')                         select pen
     d clear_key       c                   const(X'BD')                         clear key
     d enter_key       c                   const(X'F1')                         enter key
     d help_key        c                   const(X'F3')                         help key
     d rolldn_key      c                   const(X'F4')                         roll down key
     d rollup_key      c                   const(X'F5')                         roll up key
     d print_key       c                   const(X'F6')                         print key
     d home_key        c                   const(X'F8')                         home key

      * ---------------------------------------------------- Local Data Area
     d*copy k3s_c030
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
     d lda_slview                     1p 0                                      order summary mode
     d lda_cnv                       10                                         libr - conversion
     d lda_dly                       10                                         libr - daily intrfac
     d lda_dta                       10                                         libr - data files
     d lda_mod                       10                                         libr - modifications
     d lda_obj                       10                                         libr - objects
     d lda_qry                       10                                         libr - queries

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

      * ---------------------------------- Message logic for screen programs
     d*copy k3s_c050
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
      **   Desc: Fields used in screen programs 'D' specifications
      **
      *****************************************************************

     d @msgid          s              7    dim(10)                              message ID
     d @msgtext        s            100    dim(10)                              message text

     d @msgfile        s             10    inz('K3S_MSGF ')                     message file
     d @mn             s              3  0                                      message number

     d @msg_id         s              7                                         message ID
     d @msg_text       s            100                                         message text

     d                 ds
     d @records                       5                                         number of records
     d  @records1                     1    overlay(@records:1)                  records overlay 1
     d  @records2                     1    overlay(@records:2)                  records overlay 2
     d  @records3                     1    overlay(@records:3)                  records overlay 3
     d  @records4                     1    overlay(@records:4)                  records overlay 4

     d @num_recds      s              5  0                                      number of records

     d @returned       s              3  0
     d @updated        s              1  0
     d @usr_updat      s              1  0                                      user update flag

     d @no_recs        c                   const('no records')                  no records for subfl
     d @all_recs       c                   const('Blank for all')               chose all records
     d @user_exit      c                   const('F8=')                         user exit function

     d @recd_frmt      s              2                                         record format

     d @mod_add        c                   const('  add   ')                    Add mode
     d @mod_chg        c                   const(' change ')                    Change mode
     d @mod_view       c                   const('  view  ')                    View mode
     d @mod_delt       c                   const(' delete ')                    Delete mode
     d @mod_copy       c                   const('  copy  ')                    Copy mode

     d @blanks         s             30                                         title clear
     d @no_title       c                   const('  Title record missing!     ')title missing
     d @start          s              3  0                                      title start
     d @length         s              3  0                                      title length
     d @F3_warn        s              1                                         F3=Exit warning

      ** fields for plist with F8= user exit logic
     d @8_program      s             10                                         program exited
     d @8_format       s             10                                         format  exited
     d @8_user         s             10                                         user
     d @8_comp         s              1                                         company
     d @8_locn         s              5                                         location
     d @8_buyr         s              5                                         buy group
     d @8_supl         s             10                                         supplier
     d @8_suplsub      s             10                                         sub supplier
     d @8_prod         s             25                                         product


      * ---------------------------------- Screen display headings
     d*copy k3s_c060
      *****************************************************************
      **
      **   K3S-Replenish - Inventory REPLENISHment System
      **   Copyright (c) 1996-1997 by King III Solutions, Inc.
      **   All rights reserved.
      **
      *****************************************************************
      **
      **   Name: K3S_C060
      **   Type: ILE /COPY member
      **   Desc: Constants for supplier units
      **
      *****************************************************************

     d @dsp_type1      c                   const('1 Reg cost')                  Regular cost
     d @dsp_type2      c                   const('2 Net cost')                  Regular cost
     d @dsp_type3      c                   const('3 Weight..')                  Regular cost
     d @dsp_type4      c                   const('4 Volume..')                  Regular cost
     d @dsp_type5      c                   const('5 Purc inc')                  Regular cost

      * ---------------------------------- Parameter values for notes system
     d*copy k3s_c150
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
      **   Desc: Notes system - parameter fields         'D' specs only
      **
      *****************************************************************

      * work fields used by /COPY member K3S_C151 and K3S_C152 start with
      *                                                      prefix of @n_

     d @n_comp         s              1                                         note type value
     d @n_typval       s              1                                         note type value
     d @n_locn         s              5                                         location
     d @n_supl         s             10                                         supplier
     d @n_suplsub      s             10                                         sub supplier
     d @n_prod         s             25                                         product
     d @n_seasonl      s             10                                         seasonal profile
     d @n_deal         s              7                                         deal id
     d @n_user         s             10                                         user id
     d @n_birth        s             10a   inz('0001-01-01')                    user id
     d @n_typnote      s              1                                         note type
     d @n_compx        s                   like(nt_comp)                        save buy group id
     d @n_locnx        s                   like(nt_locn)                        save buy group id
     d @n_locnxx       s                   like(nt_locn)                        save buy group id
     d @n_typvalx      s                   like(nt_typval)                      save buy group id
     d @n_suplx        s                   like(nt_supl)                        save buy group id
     d @n_suplsux      s                   like(nt_suplsub)                     save buy group id
     d @n_prodx        s                   like(nt_prod)                        save buy group id
     d @n_dealx        s                   like(nt_deal)                        save buy group id
     d @n_notex        s                   like(nt_note)                        save buy group id
     d @n_seasonx      s                   like(nt_seasonl)                     save buy group id
     d @n_typnotx      s                   like(nt_typnote)                     save buy group id
     d @n_sequenx      s                   like(nt_sequenc)                     save buy group id
     d @n_notecx       s              1                                         errors detected ?
     d @n_okx          s              1                                         errors detected ?
     d @n_contx        s              3  0                                      errors detected ?
     d @n_suplz        s                   like(nt_supl)                        save buy group id
     d @n_suplsuz      s                   like(nt_suplsub)                     save buy group id
     d @n_timstmp      s               z   inz
     d @n_time         s               t   timfmt(*iso)
     d @n_date         s               d   datfmt(*iso)
     d @n_birthtm      s               t   timfmt(*iso)
     d @n_birthdy      s               d   datfmt(*iso)
     d @n_chgtype      s              1
     d @n_userchg      s             10
     d @n_workstn      s             10
     d @n_program      s             10

     d inds            ds
     d  protect_flds          30     30n                                        work station id
     d  comb_supl             35     35n                                        work station id
     d  pc_zz_begin           58     58n                                        work station id
     d  pc_zz_end             59     59n                                        work station id
     d  pc_zz_arvdate         60     60n                                        work station id
     d  pc_zz_hldreas         61     61n                                        work station id
     d  ul_zz_dater           95     95n                                        work station id
     d  ul_zz_timer           96     96n                                        work station id

      * -------------------------------------------------- parameters passed
     d soqseq#x        s              5a                                        batch number
     d screenvmi       s              1a   inz(' ')                             location
     d openscrn        s              1a   inz(' ')                             location
     d vnd5            s              5s 0
     d workstatn       s             10a                                        location
     d savpo#          s                   like(so_po#)                         location
     d zz_narcot       s             10a                                        location
     d zz_codetyp      s                   like(ta_codetyp)                     location
     d zz_codeval      s                   like(ta_codeval)                     location
     d zz_buyr         s                   like(so_buyr)                        location
     d zz_locn         s                   like(so_locn)                        location
     d xx_locnfrm      s                   like(so_locn)                        location
     d xx_locnto       s                   like(so_locn)                        location
     d zz_sub          s                   like(so_suplsub)                     location
     d zz_supl         s                   like(so_supl)                        location
     d zz_suplsub      s                   like(so_suplsub)                     location
     d zz_note         s                   like(nt_note)                        location
     d zz_noteflg      s              2                                         location
     d blank           s                   like(lc_comp)                        location
     d flag3_test      s              1  0                                      location

      * --------------------------------------------------------- Workfields
     d save_locn       s                   like(lc_locn)                        save location id
     d hold_locn       s                   like(lc_locn)                        save location id
     d save_supl       s                   like(sp_supl)                        save supplier id
     d save_sub        s                   like(sp_suplsub)                     save sub supplier id
     d save_buyr       s                   like(so_buyr)                        save buy group id
     d fxc_data        s              1                                         errors detected ?
     d vfaxcd          s              1                                         errors detected ?
     d zz_skiplt       s              1  0                                      errors detected ?
     d usenote         s              1    inz(' ')                             errors detected ?

     d xx_date         s             10a                                        errors detected ?
     d batchtype       s              1                                         errors detected ?
     d errors          s              1                                         errors detected ?
     d onetime         s              1                                         errors detected ?
     d chg_file        s              1                                         errors detected ?
     d cur_1typ        s              1a                                        errors detected ?
     d zz_process      s              1                                         errors detected ?
     d send_error      s              1                                         errors detected ?
     d re_display      s              1                                         re-display ?
     d finished        s              1                                         Deal added, get out
     d char_date       s             10                                         returned date frmtd

     d not_six         s              1p 0 inz(0)                               Date formated, not 6

     d iso_error       s              1p 0                                      Error date entered
     d iso_begin       s               d   datfmt(*iso)                         batch begin date
     d iso_end         s               d   datfmt(*iso)                         batch end date
     d xx_arvdate      s               d   datfmt(*iso)                         batch begin date
     d xx_begin        s               d   datfmt(*iso)                         batch begin date
     d xx_end          s               d   datfmt(*iso)                         batch begin date
      * --------------------------------------------------- parameter passed prototype
     d/copy qprotosrc
      * --------------------------------------------------- parameter passed
     d K3S_1050AU      PI
     d  comp                          1
     d  locn                          5
     d  buyr                          5
     d  supl                         10
     d  suplsub                      10
     d  soqseq#                       5  0
     d  sopo#                        10
     d  mode                          8
     d  @updated                      3  0
     d  @returned                     1  0
     d*
      * -------------------------------------- Likerec Statements
     d notepada_key    DS                  likerec(rk_notepad:*key)
     d suplsoqa_key    DS                  likerec(rk_suplsoq:*key)
     d suplpurb_key    DS                  likerec(rk_suplpur:*key)
      * ---------------------------------------------------------- Key Lists
      /free
       //------------------------------------------------------ Once Routine
       //once routine
       exsr $_once;

       //set off error
       errors = *off;

       zz_pomsg1 = sp_supl + ' ' + sp_name;
       zz_pomsg2 = 'Automatic Purchase order';
       zz_pomsg3 = 'PO# used : ' + savpo#;
       zz_pomsg4 = 'Created by K3S_NIGHT    ';
       zz_pomsg5 = 'Purchase order generated';
       zz_pomsg6 = 'without buyer review    ';
       zz_pomsg7 = 'or approval process     ';

       //   edit buy group entry
       exsr $_edt_buyr;
       //   edit request type
       if errors = *off;
          exsr $_edt_type;
       endif;
       //   edit location entry
       if errors = *off;
          exsr $_edt_locn;
       endif;
       //   edit supplier entry
       if errors = *off;
          exsr $_edt_supl;
       endif;
       //   edit user entry
       if errors = *off;
          exsr $_edt_beg;
       endif;
       //   edit user entry
       if errors = *off;
          exsr $_edt_end;
       endif;
       //   edit user entry
       if errors = *off;
          exsr $_edt_reas;
       endif;
       //if errors exist, pass error message
       if errors = *on;
          exsr $_pass_msg;
       endif;

       //   accept supplier po
       if (errors = *off);
          usenote = 'Y';
          chain (lda_comp:sp_supl:sp_suplsub) k_suplapva;
          if %found(k_suplapva);
             if zz_pomsg1 = sv_coment1 and
                zz_pomsg2 = sv_coment2 and
                zz_pomsg3 = sv_coment3 and
                zz_pomsg4 = sv_coment4 and
                zz_pomsg5 = sv_coment5 and
                zz_pomsg6 = sv_coment6 and
                zz_pomsg7 = sv_coment7;
                   sv_lastupd = lda_cmpdat;
             endif;
             if zz_pomsg1 <> sv_coment1 or
                zz_pomsg2 <> sv_coment2 or
                zz_pomsg3 <> sv_coment3 or
                zz_pomsg4 <> sv_coment4 or
                zz_pomsg5 <> sv_coment5 or
                zz_pomsg6 <> sv_coment6 or
                zz_pomsg7 <> sv_coment7;
                if zz_pomsg1 <> *blanks;
                   sv_coment1 = zz_pomsg1;
                   sv_lastupd = lda_cmpdat;
                endif;
                if zz_pomsg2 <> *blanks;
                   sv_coment2 = zz_pomsg2;
                   sv_lastupd = lda_cmpdat;
                endif;
                if zz_pomsg3 <> *blanks;
                   sv_coment3 = zz_pomsg3;
                   sv_lastupd = lda_cmpdat;
                endif;
                if zz_pomsg4 <> *blanks;
                   sv_coment4 = zz_pomsg4;
                   sv_lastupd = lda_cmpdat;
                endif;
                if zz_pomsg5 <> *blanks;
                   sv_coment5 = zz_pomsg5;
                   sv_lastupd = lda_cmpdat;
                endif;
                if zz_pomsg6 <> *blanks;
                   sv_coment6 = zz_pomsg6;
                   sv_lastupd = lda_cmpdat;
                endif;
                if zz_pomsg7 <> *blanks;
                   sv_coment7 = zz_pomsg7;
                   sv_lastupd = lda_cmpdat;
                endif;
             endif;
             update    rk_suplapv;
       else;
          sv_coment1 = zz_pomsg1;
          sv_coment2 = zz_pomsg2;
          sv_coment3 = zz_pomsg3;
          sv_coment4 = zz_pomsg4;
          sv_coment5 = zz_pomsg5;
          sv_coment6 = zz_pomsg6;
          sv_coment7 = zz_pomsg7;
          sv_lastupd = lda_cmpdat;
          sv_birth   = lda_cmpdat;
          sv_comp    = lda_comp;
          sv_supl    = sp_supl;
          sv_suplsub = sp_suplsub;
          write     rk_suplapv;
       endif;
       suplsoqa_key.so_comp = lda_comp;
       suplsoqa_key.so_buyr = buyr;
       suplsoqa_key.so_locn = locn;
       suplsoqa_key.so_supl = supl;
       suplsoqa_key.so_suplsub = suplsub;
       suplsoqa_key.so_soqseq# = soqseq#;
       chain %kds(suplsoqa_key) k_suplsoqa;
       if (so_altsrce = 1) and
          (so_soqtype <> 'PN');
             so_soqtype = 'PN';

       //    print pro forma
             callp K3S_1090CL(lda_comp:
                             so_buyr:
                             so_locn:
                             so_supl:
                             so_suplsub:
                             so_soqseq#:
                             zz_ponum:
                             zz_pomsg1:
                             zz_pomsg2:
                             zz_pomsg3:
                             zz_pomsg4:
                             zz_pomsg5:
                             zz_pomsg6:
                             zz_pomsg7);

       else;
          chain (lda_comp:buyr:locn:so_birth) k_buyrsota;
          if %found(k_buyrsota);
             select;
       //Fixed cycle
                when so_fxcfrq > 0;
                   bt_duefc#r = bt_duefc#r - 1;
                   bt_duefcvr = bt_duefcvr - so_actunet;
                   bt_aprov# = bt_aprov# + 1;
                   bt_aprovv = bt_aprovv + so_actunet;
                   update    rk_buyrsot;
       //Reminder
                when so_chkremi > 0;
                   bt_duerm#r = bt_duerm#r - 1;
                   bt_duermvr = bt_duermvr - so_actunet;
                   bt_aprov# = bt_aprov# + 1;
                   bt_aprovv = bt_aprovv + so_actunet;
                   update    rk_buyrsot;
       //Forward buy
                when so_chkfbuy > 0;
                   bt_duefb#r = bt_duefb#r - 1;
                   bt_duefbvr = bt_duefbvr - so_actunet;
                   bt_aprov# = bt_aprov# + 1;
                   bt_aprovv = bt_aprovv + so_actunet;
                   update    rk_buyrsot;
       //No Delay
                when so_joint = 0;
                   bt_duend#r = bt_duend#r - 1;
                   bt_duendvr = bt_duendvr - so_actunet;
                   bt_aprov# = bt_aprov# + 1;
                   bt_aprovv = bt_aprovv + so_actunet;
                   update    rk_buyrsot;
                other;
       //Not due
                   bt_notdu#r = bt_notdu#r - 1;
                   bt_notduvr = bt_notduvr - so_actunet;
                   bt_aprov# = bt_aprov# + 1;
                   bt_aprovv = bt_aprovv + so_actunet;
                   update    rk_buyrsot;
             endsl;
          endif;
          so_soqtype = 'AP';
       // zz_faxpo = ' ';
       //                if        sppfaxord = 1
       //                eval      vfaxcd = 'Y'
       //                else
       //                eval      vfaxcd = ' '
       //                endif
       endif;
       so_po# = savpo#;                                                         //note type value
       so_ordate = lda_cmpdat;
       so_expdelv = xx_arvdate;
       update    rk_suplsoq;                                                    //location id
       // create po interface records header & detail

       // get supplier record
       sp_comp    = lda_comp;
       sp_locn    = locn;
       sp_supl    = supl;
       sp_suplsub = suplsub;
       chain (sp_comp:sp_locn:sp_supl:sp_suplsub) k_supliera;
       //-------------------------------------------------------------------
       //  fixed cycle logic
       if sp_fxcfrq > 0 AND
          sp_fxcnxt = lda_cmpdat;
             sp_fxclst = lda_cmpdat;
             if sp_fxcfrq = 1;
                sp_fxcnxt = lda_cmpdat + %days(7);
             endif;
             if sp_fxcfrq = 2;
                sp_fxcnxt = lda_cmpdat + %days(14);
             endif;
             if sp_fxcfrq = 3;
                sp_fxcnxt = lda_cmpdat + %days(21);
             endif;
             if sp_fxcfrq = 4;
                sp_fxcnxt = lda_cmpdat + %days(28);
             endif;
             if sp_fxcfrq = 5;
                sp_fxcnxt = lda_cmpdat + %days(35);
             endif;
             if sp_fxcfrq = 6;
                sp_fxcnxt = sp_fxcnxt + %days(42);
             endif;
       endif;
       //-------------------------------------------------------------------

       if sp_fxcfrq > 0 AND
          sp_fxcnxt < lda_cmpdat;
             sp_fxclst = lda_cmpdat;
             if sp_fxcfrq = 1;
                sp_fxcnxt = sp_fxcnxt + %days(7);
             endif;
             if sp_fxcfrq = 2;
                sp_fxcnxt = sp_fxcnxt + %days(14);
             endif;
             if sp_fxcfrq = 3;
                sp_fxcnxt = sp_fxcnxt + %days(21);
             endif;
             if sp_fxcfrq = 4;
                sp_fxcnxt = sp_fxcnxt + %days(28);
             endif;
             if sp_fxcfrq = 5;
                sp_fxcnxt = sp_fxcnxt + %days(35);
             endif;
             if sp_fxcfrq = 6;
                sp_fxcnxt = sp_fxcnxt + %days(42);
             endif;
       endif;
       //-------------------------------------------------------------------

       sp_ordate = lda_cmpdat;
       clear sp_usern1;
       update    rk_suplier;                                                     //location id
       if so_soqtype = 'AP';
          if zz_prthere = ' ';
             zz_prthere = '0';
          endif;
          workstatn = 'AUTO_PO   ';
          callp K3S_1100(lda_comp:
                        so_buyr:
                        so_locn:
                        so_supl:
                        so_suplsub:
                        so_soqseq#:
                        zz_pomsg1:
                        zz_pomsg2:
                        zz_pomsg3:
                        zz_pomsg4:
                        zz_pomsg5:
                        zz_pomsg6:
                        zz_pomsg7:
                        zz_prthere:
                        lda_cmpdat:
                        xx_arvdate:
                        zz_narcot:
                        zz_potype:
                        xx_locnfrm:
                        xx_locnto:
                        workstatn:
                        xx_begin:
                        xx_end:
                        zz_hldreas:
                        zz_criticl:
                        zz_loadcod:
                        zz_skiplt);
          if cm_approve = 0 and
             so_suplsub <> 'h';
                exsr $_add_pur;
          endif;
          if cm_approve = 0 and
             so_suplsub = 'h';
                exsr $_add_purc;
          endif;
       endif;                                                                   //supplier id

       finished = *on;
       endif;                                                                   //supplier id

       //end of main loop

       //--------------------------------------------------- End of Main Loop

       //finished, set on LR
       *inlr = *on;
       //return

       //**************************************************** End of program

       //////////////////////////////////////////////////////// Once routine

       begsr $_once;

       if openscrn  <> ' ';
          open K3s_1050fm;
       endif;
       if screenvmi <> ' ';
          write     k3_ctl_msg;
          exfmt     K3_1050_1r;
       endif;
       zz_criticl = 0;
       zz_loadcod = ' ';
       zz_skiplt = 0;
       @n_typval = 'H';
       @n_locn = locn;                                                          //location id
       @n_supl = supl;                                                          //supplier id
       @n_suplsub = suplsub;                                                    //sub supplier id
       @n_suplz = *blanks;                                                      //supplier id
       @n_suplsuz = *blanks;                                                    //sub supplier id

       if (mode = ' delete ') or
          (mode = '  view  ');
              pc_zz_begin = *on;
              protect_flds = *off;
              zz_hldreas = *blanks;
              zz_begin   = *blanks;
              zz_end     = *blanks;
              pc_zz_end = *off;
              onetime = '1';
       endif;
       if mode = '  add   ';
          onetime = ' ';
          pc_zz_end = *on;
          buyr    = *blanks;
          locn    = *blanks;
          supl    = *blanks;
          suplsub = *blanks;
       endif;

       //prime user id
       zz_user = psds_user;
       //eval      poref   = *blanks
       //eval      k3ship = '1'
       //eval      k3desc  = 'Highway 42 East          '

       //prime buy group id
       so_buyr   = buyr;
       zz_buyr   = buyr;
       save_buyr = buyr;

       //prime location id
       so_locn   = locn;
       sa_locn   = locn;
       sp_locn   = locn;
       zz_locn   = locn;
       save_locn = locn;

       //prime supplier id
       vnd5 = %dec(%subst(supl:1:5):5:0);
       zz_supl   = supl;
       save_supl = supl;
       sp_supl = supl;
       sa_supl = supl;

       // prime sub supplier id
       zz_sub = suplsub;
       save_sub   = suplsub;
       sp_suplsub = suplsub;
       sa_suplsub = suplsub;

0152   //  sp_key        chain     k_suplpoda                         35
       //                eval      zz_faxpo = 'N'
       //                eval      *in36 = *off
       //                eval      *in37 = *off
       //                if        comb_supl = *off;
       //                if        sppfaxord = 1 or
       //                          sppediord = 1
       //                eval      zz_faxpo = 'Y'
       //                if        sppfaxord = 1
       //                eval      *in36 = *on
       //                eval      *in37 = *off
       //                endif
       //                if        sppediord = 1
       //                eval      *in36 = *off
       //                eval      *in37 = *on
       //                endif
       //                endif


       //set finished indicator to off
       finished = *off;

       //set error off
       errors = *off;

       //position cursor to supplier
       *in54 = *on;

       //prime company ID
       lda_comp = comp;

       //prime program id
       zz_program = psds_progm;

       //get user update authority flag
       // call module to obtain user update authority flag
       //                call      'K3S_9050'
       //                parm                    zz_program
       //                parm      0             @usr_updat
       //                eval      *in94 =      (@usr_updat = 0)

       //get company record
       chain(n) (lda_comp) k_companya;

       if %found(k_companya);
          callp K3S_7000CL(lda_comp:
                          zz_ponum);
          savpo# = zz_ponum;
       endif;

       //prime company code
       zz_compcod = cm_compcod;

       //get batch header record
       in *dtaara;
       lda_comp = comp;
       lda_cmpdat = cm_sysdate;
       lda_sysdat = cm_sysdate;
       lda_usradj = 0;
       lda_alarm  = 0;
       lda_dealex = 0;
       lda_date6  = 0;
       lda_managr = 0;
       lda_window = 0;
       lda_orview = 0;
       lda_prview = 0;
       lda_slview = 0;
       lda_proptn = 0;
       lda_hisdsp = 0;
       lda_trcksg = .000;
       lda_trmprd = 0;
       lda_untdsp = 0;
       lda_prdsum = 0;
       lda_mltwrn = 0;
       lda_altvu  = 0;
       lda_logwrn = 0;
       lda_exnote = 0;
       lda_severe = 0;
       lda_f3_wrn = 0;
       lda_cr1010 = 0;
       lda_cstwrn = 0;
       lda_arvdat = 0;
       lda_autrnd = 0;
       out *dtaara;
       suplsoqa_key.so_comp = lda_comp;
       suplsoqa_key.so_buyr = buyr;
       suplsoqa_key.so_locn = locn;
       suplsoqa_key.so_supl = so_supl;
       suplsoqa_key.so_suplsub = suplsub;
       suplsoqa_key.so_soqseq# = soqseq#;
       chain(n) %kds(suplsoqa_key) k_suplsoqa;

       xx_date = %char(lda_cmpdat:*iso);
       if %found(k_suplsoqa);
          if (mode = ' delete ') or
             (mode = '  view  ');
       //   edit buy group entry
                 exsr d_edt_buyr;
       //   edit location entry
                 if errors = *off;
                    exsr d_edt_locn;
                 endif;
       //   edit supplier entry
                 if errors = *off;
                    exsr d_edt_supl;
                    xx_arvdate = lda_cmpdat + %days(sp_leadtmo);
                 endif;
                 chain (lda_comp:sp_locn:sp_supl:sp_suplsub) k_trancena;
                 if %found(k_trancena);
                    protect_flds = *on;
                    zz_potype = 'T';
                    xx_locnto = tc_locnto;
                    xx_locnfrm = tc_locnfrm;
                 else;
                    zz_potype = 'R';
                    xx_locnto = *blanks;
                    xx_locnfrm = *blanks;
                 endif;
                 if (so_altsrce = 1) and
                    (so_soqtype <> 'PN');
                        protect_flds  = *on;
                 endif;
       //   get po number
                 if (errors = *off) and
                    (so_soqtype <> 'PN') and
                    (sopo#  = *blanks);
                        zz_ponum = savpo#;
                        if protect_flds = *off;
                           callp K3S_7010CL(lda_comp:
                                           locn:
                                           supl:
                                           suplsub:
                                           zz_potype);
                         endif;
                        if (so_altsrce = 1) and
                           (so_soqtype <> 'PN');
                               zz_potype = 'P';
                        endif;
                 else;
                     if protect_flds = *off;
                        callp K3S_7010CL(lda_comp:
                                        locn:
                                        supl:
                                        suplsub:
                                        zz_potype);
                     endif;
                     if (so_altsrce = 1) and
                        (so_soqtype <> 'PN');
                           eval zz_potype = 'P';
                     endif;
                     zz_ponum = savpo#;
                 endif;
                 if mode = ' delete ';
                    errors    = *on;
                    @msg_id   = 'K3_9010';
                    exsr $_add_msg;
                    exsr $_pass_msg;
                 endif;
          endif;
       endif;
       so_po# = savpo#;                                                         //note type value
       @returned = *zeros;
       chain(n) (lda_comp:sp_supl:sp_suplsub) k_suplapva;
       if %found(k_suplapva);
          zz_pomsg1o = sv_coment1;
          zz_pomsg2o = sv_coment2;
          zz_pomsg3o = sv_coment3;
          zz_pomsg4o = sv_coment4;
          zz_pomsg5o = sv_coment5;
          zz_pomsg6o = sv_coment6;
          zz_pomsg7o = sv_coment7;
       else;
          zz_pomsg1 = *blanks;
          zz_pomsg2 = *blanks;
          zz_pomsg3 = *blanks;
          zz_pomsg4 = *blanks;
          zz_pomsg5 = *blanks;
          zz_pomsg6 = *blanks;
          zz_pomsg7 = *blanks;
          zz_pomsg1o = *blanks;
          zz_pomsg2o = *blanks;
          zz_pomsg3o = *blanks;
          zz_pomsg4o = *blanks;
          zz_pomsg5o = *blanks;
          zz_pomsg6o = *blanks;
          zz_pomsg7o = *blanks;
       endif;
       zz_narcot = *blanks;
       endsr;

       /////////////////////////////////////////////////////// Edit supplier

       begsr $_edt_supl;

       //prime key list for supplier
       sp_comp    = comp;
       sp_locn    = locn;
       sp_supl    = supl;
       sp_suplsub = suplsub;

       //blank supplier would mean all suppliers
       if supl   = *blanks;
          save_supl = supl;

       //otherwise, validate entry
       else;

       //get supplier record
          chain(n) (sp_comp:sp_locn:sp_supl:sp_suplsub) k_supliera;

       // supplier record not found, send back error
          if not %found(k_supliera);
             errors    = *on;
             send_error = *on;
             *in56     = *on;
             @msg_id   = 'K3_2000';
             @msg_text = supl;
             exsr $_add_msg;
          else;

       //----------------------
       // format supplier name
             if zz_suplsub = *blanks;
                zz_suplnam = %trimr(zz_supl) + ' ' +
                             sp_name;
             else;
                zz_suplnam = %trimr(zz_supl) + ' ' +
                             zz_suplsub + ' ' +
                             sp_name;
             endif;
       //----------------------

          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////////// Edit supplier

       begsr $_edt_supc;

       comb_supl = *off;

       if so_suplsub = 'h';
          comb_supl = *on;
       // prime key list for supplier
          sp_comp    = lda_comp;
          sp_locn    = so_cmblocn;
          sp_supl    = so_cmbsupl;
          sp_suplsub = so_cmbsub;
       // get supplier record
          chain(n) (sp_comp:sp_locn:sp_supl:sp_suplsub) k_supliera;

       // supplier record not found, send back error
          if not %found(k_supliera);
              errors    = *on;
              send_error = *on;
              *in56     = *on;
              @msg_id   = 'K3_2000';
              @msg_text = supl;
              exsr $_add_msg;
          else;

       //----------------------
       // format supplier name
              if so_cmbsupl = *blanks;
                 zz_suplcmb = 'Loc' + ' ' +
                              so_cmblocn + ' ' +
                              %trimr(so_cmbsupl) + ' ' +
                              sp_name;
              else;
                 zz_suplcmb = 'Loc' + ' ' +
                              so_cmblocn + ' ' +
                              %trimr(so_cmbsupl) + ' ' +
                              so_cmbsub + ' ' +
                              sp_name;
              endif;
       //----------------------

          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////////// Edit supplier

       begsr d_edt_supl;

       //prime key list for supplier
       sp_comp    = lda_comp;
       sp_supl    = supl;

       //blank supplier would mean all suppliers
       if supl   = *blanks;
          save_supl = supl;

       //otherwise, validate entry
       else;

       //get supplier record
           chain(n) (sp_comp:sp_locn:sp_supl:sp_suplsub) k_supliera;

       //supplier record not found, send back error
          if not %found(k_supliera);
             errors    = *on;
             send_error = *on;
             *in56     = *on;
             @msg_id   = 'K3_2000';
             @msg_text = supl;
             exsr $_add_msg;
          else;

       //----------------------
       //format supplier name
             if zz_suplsub = *blanks;
                zz_suplnam = %trimr(zz_supl) + ' ' +
                             sp_name;
             else;
                zz_suplnam = %trimr(zz_supl) + ' ' +
                             zz_suplsub + ' ' +
                             sp_name;
             endif;
       //----------------------

          endif;

       endif;

       endsr;



       //////////////////////////////////////////////// Edit buy group entry

       begsr $_edt_buyr;

       //prime key list for buy group
       by_comp    = lda_comp;
       by_buyr    = buyr;

       //blank buy group would mean all buy groups
       if buyr = *blanks;
          save_buyr = buyr;

       //otherwise, validate entry
       else;

          chain (by_comp:by_buyr) k_buyrgrpa;

       //    buy group does not exist
          if not %found(k_buyrgrpa);
             errors    = *on;
             send_error= *on;
             *in51     = *on;
             @msg_id   = 'K3_4050';
             @msg_text = buyr;
             exsr $_add_msg;
          else;
             zz_buyrnam = %trimr(zz_buyr) + ' ' +
                          by_name;
          endif;

       endif;

       endsr;

       ///////////////////////////////////////////////// Edit location entry

       begsr $_edt_locn;

       //prime key list to read this company only
       lc_comp    = lda_comp;
       lc_locn    = locn;

       //blank location would mean all locations
       if locn   = *blanks;
          save_locn = locn;

       //otherwise, validate entry
       else;

       //get location record
           chain (lc_comp:lc_locn) k_locatnsa;

       //    location does not exist
           if not %found(k_locatnsa);
              errors    = *on;
              send_error= *on;
              *in53     = *on;
              @msg_id   = 'K3_8020';
              @msg_text = locn;
              exsr $_add_msg;
           else;
              zz_locndes = %trimr(zz_locn) + ' ' +
                           lc_desc;
           endif;

       endif;

       endsr;

       //////////////////////////////////////////////// Edit buy group entry

       begsr d_edt_buyr;

       //prime key list for buy group
       by_comp    = lda_comp;
       by_buyr    = buyr;

       //blank buy group would mean all buy groups
       if buyr = *blanks;
          save_buyr = buyr;

       //otherwise, validate entry
       else;

       //get buy group record
          chain (by_comp:by_buyr) k_buyrgrpa;

       //buy group does not exist
          if not %found(k_buyrgrpa);
             errors    = *on;
             send_error= *on;
             *in51     = *on;
             @msg_id   = 'K3_4050';
             @msg_text = buyr;
             exsr $_add_msg;
          else;
             zz_buyrnam = %trimr(zz_buyr) + ' ' +
                          by_name;
          endif;

       endif;

       endsr;

       ///////////////////////////////////////////////// Edit location entry

       begsr d_edt_locn;

       //prime key list to read this company only
       lc_comp    = lda_comp;
       lc_locn    = locn;

       //blank location would mean all locations
       if locn   = *blanks;
          save_locn = locn;

       //otherwise, validate entry
       else;

       //get location record
          chain (lc_comp:lc_locn) k_locatnsa;

       //location does not exist
          if not %found(k_locatnsa);
             errors    = *on;
             send_error= *on;
             *in53     = *on;
             @msg_id   = 'K3_8020';
             @msg_text = locn;
             exsr $_add_msg;
          else;
             zz_locndes = %trimr(zz_locn) + ' ' +
                          lc_desc;

          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////// Edit region entry

       begsr $_edt_type;

       //prime key list to read this company only
       ta_comp    = lda_comp;
       ta_codetyp = 'POT';
       ta_codeval = zz_potype;

       //get region record in table file
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;

       //user does not exist
       if not %found(k_tablcoda);
          errors    = *on;
          send_error= *on;
          *in52     = *on;
          @msg_id   = 'K3_9350';
          @msg_text = zz_potype;
          exsr $_add_msg;
       endif;

       endsr;

       ///////////////////////////////////////////////////// Edit begin date

       begsr $_edt_reas;

       if (zz_begin   = *blanks) and
          (zz_hldreas <> *blanks) or
          (zz_end     = *blanks) and
          (zz_hldreas <> *blanks) or
          (zz_begin   = *blanks) and
          (zz_end     = *blanks) and
          (zz_hldreas <> *blanks);
             errors    = *on;
             pc_zz_begin = *on;
             @msg_id   = 'K3_9999';
             @msg_text = 'Begin & End must be entered    ';
             exsr $_add_msg;
       else;
          pc_zz_begin = *off;
       endif;

       if (zz_begin  <> *blanks) and
          (zz_end    <> *blanks) and
          (zz_hldreas =  *blanks);
             errors    = *on;
             pc_zz_hldreas = *on;
             @msg_id   = 'K3_9999';
             @msg_text = 'Hold reason must be entered    ';
             exsr $_add_msg;
       else;
             pc_zz_hldreas = *off;
       endif;

       endsr;

       ///////////////////////////////////////////////////// Edit begin date

       begsr $_edt_beg;

       //blank requested begin date would not exclude any batches
       if (zz_begin   = *blanks) and
          (zz_end     = *blanks);

       //otherwise, validate entry
       else;

       //edit requested begin date
          clear iso_begin;
       //call module to convert user entered date into *ISO format
          iso_error = 0;
          callp K3S_M130(zz_begin:
                        lda_usrdat:
                        lda_date6:
                        iso_begin:
                        iso_error);

       //requested begin date entered, not valid
          if iso_error = 1;
             errors    = *on;
             pc_zz_begin = *on;
             @msg_id   = 'K3_9030';
             @msg_text = zz_begin;
             exsr $_add_msg;
          else;
             pc_zz_begin = *off;
             xx_begin   = iso_begin;
          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////////// Edit end date

       begsr $_edt_end;

       //blank requested end date would not exclude any batches
       if (zz_end     = *blanks) and
          (zz_begin   = *blanks);

       //otherwise, validate entry
       else;

       //edit requested end date
          clear iso_end;
       //call module to convert user entered date into *ISO format
          iso_error = 0;
          callp K3S_M130(zz_end:
                        lda_usrdat:
                        lda_date6:
                        iso_end:
                        iso_error);

       //    requested end date entered, not valid
          if iso_error = 1;
             errors    = *on;
             pc_zz_end = *on;
             @msg_id   = 'K3_9040';
             @msg_text = zz_end;
             exsr $_add_msg;
          else;
             pc_zz_end = *off;
             xx_end   = iso_end;
          endif;

       // the next two tests would take place only if the user has keyed
       //     in both a begin and end date, which passed previous edits
          if (zz_begin <> *blanks) and
             (zz_end   <> *blanks) and
             (errors    = *off);

       //    requested begin date should not be greater than requested end
             if (iso_begin > iso_end);
                errors    = *on;
                pc_zz_begin = *on;
                @msg_id   = 'K3_9050';
                @msg_text = *blanks;
                exsr $_add_msg;
             else;
                pc_zz_begin = *off;
                xx_end   = iso_end;
                xx_begin   = iso_begin;
             endif;

          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////////// Add purchase

       begsr $_add_pur;

       //prime key list for supplier
       sa_comp    = lda_comp;
       sa_locn    = zz_locn;
       sa_supl    = zz_supl;
       sa_suplsub = zz_suplsub;
       xx_date = %char(lda_sysdat:*iso);
       sa_year = %dec(%subst(xx_date:1:4):4:0);

       //get supplier record
       chain %kds(suplpurb_key) k_suplpurb;
       sa_lastupd = lda_sysdat;

       suplsoqa_key.so_comp = lda_comp;
       suplsoqa_key.so_buyr = buyr;
       suplsoqa_key.so_locn = locn;
       suplsoqa_key.so_supl = supl;
       suplsoqa_key.so_suplsub = suplsub;
       suplsoqa_key.so_soqseq# = soqseq#;
       chain(n) %kds(suplsoqa_key) k_suplsoqa;
       if %found(k_suplsoqa);
          if umonth = 01;
             sa_dolr01 += so_actunet;
             sa_wght01 += so_actuwgt;
             sa_volm01 += so_actuvol;
             sa_puri01 += so_actupqt;
             sa_othr01 += so_actuoth;
             sa_#pos01 += 1;
          endif;

          if umonth = 02;
             sa_dolr02 += so_actunet;
             sa_wght02 += so_actuwgt;
             sa_volm02 += so_actuvol;
             sa_puri02 += so_actupqt;
             sa_othr02 += so_actuoth;
             sa_#pos02 += 1;
          endif;

          if umonth = 03;
             sa_dolr03 += so_actunet;
             sa_wght03 += so_actuwgt;
             sa_volm03 += so_actuvol;
             sa_puri03 += so_actupqt;
             sa_othr03 += so_actuoth;
             sa_#pos03 += 1;
          endif;

          if umonth = 04;
             sa_dolr04 += so_actunet;
             sa_wght04 += so_actuwgt;
             sa_volm04 += so_actuvol;
             sa_puri04 += so_actupqt;
             sa_othr04 += so_actuoth;
             sa_#pos04 += 1;
          endif;

          if umonth = 05;
             sa_dolr05 += so_actunet;
             sa_wght05 += so_actuwgt;
             sa_volm05 += so_actuvol;
             sa_puri05 += so_actupqt;
             sa_othr05 += so_actuoth;
             sa_#pos05 += 1;
          endif;

          if umonth = 06;
             sa_dolr06 += so_actunet;
             sa_wght06 += so_actuwgt;
             sa_volm06 += so_actuvol;
             sa_puri06 += so_actupqt;
             sa_othr06 += so_actuoth;
             sa_#pos06 += 1;
          endif;

          if umonth = 07;
             sa_dolr07 += so_actunet;
             sa_wght07 += so_actuwgt;
             sa_volm07 += so_actuvol;
             sa_puri07 += so_actupqt;
             sa_othr07 += so_actuoth;
             sa_#pos07 += 1;
          endif;

          if umonth = 08;
             sa_dolr08 += so_actunet;
             sa_wght08 += so_actuwgt;
             sa_volm08 += so_actuvol;
             sa_puri08 += so_actupqt;
             sa_othr08 += so_actuoth;
             sa_#pos08 += 1;
          endif;

          if umonth = 09;
             sa_dolr09 += so_actunet;
             sa_wght09 += so_actuwgt;
             sa_volm09 += so_actuvol;
             sa_puri09 += so_actupqt;
             sa_othr09 += so_actuoth;
             sa_#pos09 += 1;
          endif;

          if umonth = 10;
             sa_dolr10 += so_actunet;
             sa_wght10 += so_actuwgt;
             sa_volm10 += so_actuvol;
             sa_puri10 += so_actupqt;
             sa_othr10 += so_actuoth;
             sa_#pos10 += 1;
          endif;

          if umonth = 11;
             sa_dolr11 += so_actunet;
             sa_wght11 += so_actuwgt;
             sa_volm11 += so_actuvol;
             sa_puri11 += so_actupqt;
             sa_othr11 += so_actuoth;
             sa_#pos11 += 1;
          endif;

          if umonth = 12;
             sa_dolr12 += so_actunet;
             sa_wght12 += so_actuwgt;
             sa_volm12 += so_actuvol;
             sa_puri12 += so_actupqt;
             sa_othr12 += so_actuoth;
             sa_#pos12 += 1;
          endif;
       else;
          sa_dolr01 = 0;
          sa_wght01 = 0;
          sa_volm01 = 0;
          sa_puri01 = 0;
          sa_othr01 = 0;
          sa_#pos01 = 0;
          sa_dolr02 = 0;
          sa_wght02 = 0;
          sa_volm02 = 0;
          sa_puri02 = 0;
          sa_othr02 = 0;
          sa_#pos02 = 0;
          sa_dolr03 = 0;
          sa_wght03 = 0;
          sa_volm03 = 0;
          sa_puri03 = 0;
          sa_othr03 = 0;
          sa_#pos03 = 0;
          sa_dolr04 = 0;
          sa_wght04 = 0;
          sa_volm04 = 0;
          sa_puri04 = 0;
          sa_othr04 = 0;
          sa_#pos04 = 0;
          sa_dolr05 = 0;
          sa_wght05 = 0;
          sa_volm05 = 0;
          sa_puri05 = 0;
          sa_othr05 = 0;
          sa_#pos05 = 0;
          sa_dolr06 = 0;
          sa_wght06 = 0;
          sa_volm06 = 0;
          sa_puri06 = 0;
          sa_othr06 = 0;
          sa_#pos06 = 0;
          sa_dolr07 = 0;
          sa_wght07 = 0;
          sa_volm07 = 0;
          sa_puri07 = 0;
          sa_othr07 = 0;
          sa_#pos07 = 0;
          sa_dolr08 = 0;
          sa_wght08 = 0;
          sa_volm08 = 0;
          sa_puri08 = 0;
          sa_othr08 = 0;
          sa_#pos08 = 0;
          sa_dolr09 = 0;
          sa_wght09 = 0;
          sa_volm09 = 0;
          sa_puri09 = 0;
          sa_othr09 = 0;
          sa_#pos09 = 0;
          sa_dolr10 = 0;
          sa_wght10 = 0;
          sa_volm10 = 0;
          sa_puri10 = 0;
          sa_othr10 = 0;
          sa_#pos10 = 0;
          sa_dolr11 = 0;
          sa_wght11 = 0;
          sa_volm11 = 0;
          sa_puri11 = 0;
          sa_othr11 = 0;
          sa_#pos11 = 0;
          sa_dolr12 = 0;
          sa_wght12 = 0;
          sa_volm12 = 0;
          sa_puri12 = 0;
          sa_othr12 = 0;
          sa_#pos12 = 0;
          if umonth = 01;
             sa_dolr01 = so_actunet;
             sa_wght01 = so_actuwgt;
             sa_volm01 = so_actuvol;
             sa_puri01 = so_actupqt;
             sa_othr01 = so_actuoth;
             sa_#pos01 = 1;
          endif;

          if umonth = 02;
             sa_dolr02 =  so_actunet;
             sa_wght02 =  so_actuwgt;
             sa_volm02 =  so_actuvol;
             sa_puri02 =  so_actupqt;
             sa_othr02 =  so_actuoth;
             sa_#pos02 =  1;
          endif;

          if umonth = 03;
             sa_dolr03 =  so_actunet;
             sa_wght03 =  so_actuwgt;
             sa_volm03 =  so_actuvol;
             sa_puri03 =  so_actupqt;
             sa_othr03 =  so_actuoth;
             sa_#pos03 =  1;
          endif;

          if umonth = 04;
             sa_dolr04 =  so_actunet;
             sa_wght04 =  so_actuwgt;
             sa_volm04 =  so_actuvol;
             sa_puri04 =  so_actupqt;
             sa_othr04 =  so_actuoth;
             sa_#pos04 =  1;
          endif;

          if umonth = 05;
             sa_dolr05 =  so_actunet;
             sa_wght05 =  so_actuwgt;
             sa_volm05 =  so_actuvol;
             sa_puri05 =  so_actupqt;
             sa_othr05 =  so_actuoth;
             sa_#pos05 =  1;
          endif;

          if umonth = 06;
             sa_dolr06 =  so_actunet;
             sa_wght06 =  so_actuwgt;
             sa_volm06 =  so_actuvol;
             sa_puri06 =  so_actupqt;
             sa_othr06 =  so_actuoth;
             sa_#pos06 =  1;
          endif;

          if umonth = 07;
             sa_dolr07 =  so_actunet;
             sa_wght07 =  so_actuwgt;
             sa_volm07 =  so_actuvol;
             sa_puri07 =  so_actupqt;
             sa_othr07 =  so_actuoth;
             sa_#pos07 =  1;
          endif;

          if umonth = 08;
             sa_dolr08 =  so_actunet;
             sa_wght08 =  so_actuwgt;
             sa_volm08 =  so_actuvol;
             sa_puri08 =  so_actupqt;
             sa_othr08 =  so_actuoth;
             sa_#pos08 =  1;
          endif;

          if umonth = 09;
             sa_dolr09 =  so_actunet;
             sa_wght09 =  so_actuwgt;
             sa_volm09 =  so_actuvol;
             sa_puri09 =  so_actupqt;
             sa_othr09 =  so_actuoth;
             sa_#pos09 =  1;
          endif;

          if umonth = 10;
             sa_dolr10 =  so_actunet;
             sa_wght10 =  so_actuwgt;
             sa_volm10 =  so_actuvol;
             sa_puri10 =  so_actupqt;
             sa_othr10 =  so_actuoth;
             sa_#pos10 =  1;
          endif;

          if umonth = 11;
             sa_dolr11 =  so_actunet;
             sa_wght11 =  so_actuwgt;
             sa_volm11 =  so_actuvol;
             sa_puri11 =  so_actupqt;
             sa_othr11 =  so_actuoth;
             sa_#pos11 =  1;
          endif;

          if umonth = 12;
             sa_dolr12 =  so_actunet;
             sa_wght12 =  so_actuwgt;
             sa_volm12 =  so_actuvol;
             sa_puri12 =  so_actupqt;
             sa_othr12 =  so_actuoth;
             sa_#pos12 =  1;
          endif;
       endif;

       //supplier record not found, add it
       if not %found(k_suplpurb);
          sa_birth = lda_sysdat;
          write rk_suplpur;
       else;
          update rk_suplpur;
       endif;


       endsr;

       /////////////////////////////////////////////////////// Add purchase

       begsr $_add_purc;

       //prime key list for supplier
       sa_comp    = lda_comp;
       sa_locn    = so_cmblocn;
       sa_supl    = so_cmbsupl;
       sa_suplsub = so_cmbsub;
       xx_date = %char(lda_sysdat:*iso);
       sa_year = %dec(%subst(xx_date:1:4):4:0);

       //get supplier record
       chain %kds(suplpurb_key) k_suplpurb;
       sa_lastupd = lda_sysdat;

       suplsoqa_key.so_comp = lda_comp;
       suplsoqa_key.so_buyr = buyr;
       suplsoqa_key.so_locn = locn;
       suplsoqa_key.so_supl = supl;
       suplsoqa_key.so_suplsub = suplsub;
       suplsoqa_key.so_soqseq# = soqseq#;
       chain(n) %kds(suplsoqa_key) k_suplsoqa;
       if %found(k_suplpurb);
          if umonth = 01;
             sa_dolr01 += so_actunet;
             sa_wght01 += so_actuwgt;
             sa_volm01 += so_actuvol;
             sa_puri01 += so_actupqt;
             sa_othr01 += so_actuoth;
             sa_#pos01 += 1;
          endif;

          if umonth = 02;
             sa_dolr02 += so_actunet;
             sa_wght02 += so_actuwgt;
             sa_volm02 += so_actuvol;
             sa_puri02 += so_actupqt;
             sa_othr02 += so_actuoth;
             sa_#pos02 += 1;
          endif;

          if umonth = 03;
             sa_dolr03 += so_actunet;
             sa_wght03 += so_actuwgt;
             sa_volm03 += so_actuvol;
             sa_puri03 += so_actupqt;
             sa_othr03 += so_actuoth;
             sa_#pos03 += 1;
          endif;

          if umonth = 04;
             sa_dolr04 += so_actunet;
             sa_wght04 += so_actuwgt;
             sa_volm04 += so_actuvol;
             sa_puri04 += so_actupqt;
             sa_othr04 += so_actuoth;
             sa_#pos04 += 1;
          endif;

          if umonth = 05;
             sa_dolr05 += so_actunet;
             sa_wght05 += so_actuwgt;
             sa_volm05 += so_actuvol;
             sa_puri05 += so_actupqt;
             sa_othr05 += so_actuoth;
             sa_#pos05 += 1;
          endif;

          if umonth = 06;
             sa_dolr06 += so_actunet;
             sa_wght06 += so_actuwgt;
             sa_volm06 += so_actuvol;
             sa_puri06 += so_actupqt;
             sa_othr06 += so_actuoth;
             sa_#pos06 += 1;
          endif;

          if umonth = 07;
             sa_dolr07 += so_actunet;
             sa_wght07 += so_actuwgt;
             sa_volm07 += so_actuvol;
             sa_puri07 += so_actupqt;
             sa_othr07 += so_actuoth;
             sa_#pos07 += 1;
          endif;

          if umonth = 08;
             sa_dolr08 += so_actunet;
             sa_wght08 += so_actuwgt;
             sa_volm08 += so_actuvol;
             sa_puri08 += so_actupqt;
             sa_othr08 += so_actuoth;
             sa_#pos08 += 1;
          endif;

          if umonth = 09;
             sa_dolr09 += so_actunet;
             sa_wght09 += so_actuwgt;
             sa_volm09 += so_actuvol;
             sa_puri09 += so_actupqt;
             sa_othr09 += so_actuoth;
             sa_#pos09 += 1;
          endif;

          if umonth = 10;
             sa_dolr10 += so_actunet;
             sa_wght10 += so_actuwgt;
             sa_volm10 += so_actuvol;
             sa_puri10 += so_actupqt;
             sa_othr10 += so_actuoth;
             sa_#pos10 += 1;
          endif;

          if umonth = 11;
             sa_dolr11 += so_actunet;
             sa_wght11 += so_actuwgt;
             sa_volm11 += so_actuvol;
             sa_puri11 += so_actupqt;
             sa_othr11 += so_actuoth;
             sa_#pos11 += 1;
          endif;

          if umonth = 12;
             sa_dolr12 += so_actunet;
             sa_wght12 += so_actuwgt;
             sa_volm12 += so_actuvol;
             sa_puri12 += so_actupqt;
             sa_othr12 += so_actuoth;
             sa_#pos12 += 1;
          endif;
       else;
          sa_dolr01 = 0;
          sa_wght01 = 0;
          sa_volm01 = 0;
          sa_puri01 = 0;
          sa_othr01 = 0;
          sa_#pos01 = 0;
          sa_dolr02 = 0;
          sa_wght02 = 0;
          sa_volm02 = 0;
          sa_puri02 = 0;
          sa_othr02 = 0;
          sa_#pos02 = 0;
          sa_dolr03 = 0;
          sa_wght03 = 0;
          sa_volm03 = 0;
          sa_puri03 = 0;
          sa_othr03 = 0;
          sa_#pos03 = 0;
          sa_dolr04 = 0;
          sa_wght04 = 0;
          sa_volm04 = 0;
          sa_puri04 = 0;
          sa_othr04 = 0;
          sa_#pos04 = 0;
          sa_dolr05 = 0;
          sa_wght05 = 0;
          sa_volm05 = 0;
          sa_puri05 = 0;
          sa_othr05 = 0;
          sa_#pos05 = 0;
          sa_dolr06 = 0;
          sa_wght06 = 0;
          sa_volm06 = 0;
          sa_puri06 = 0;
          sa_othr06 = 0;
          sa_#pos06 = 0;
          sa_dolr07 = 0;
          sa_wght07 = 0;
          sa_volm07 = 0;
          sa_puri07 = 0;
          sa_othr07 = 0;
          sa_#pos07 = 0;
          sa_dolr08 = 0;
          sa_wght08 = 0;
          sa_volm08 = 0;
          sa_puri08 = 0;
          sa_othr08 = 0;
          sa_#pos08 = 0;
          sa_dolr09 = 0;
          sa_wght09 = 0;
          sa_volm09 = 0;
          sa_puri09 = 0;
          sa_othr09 = 0;
          sa_#pos09 = 0;
          sa_dolr10 = 0;
          sa_wght10 = 0;
          sa_volm10 = 0;
          sa_puri10 = 0;
          sa_othr10 = 0;
          sa_#pos10 = 0;
          sa_dolr11 = 0;
          sa_wght11 = 0;
          sa_volm11 = 0;
          sa_puri11 = 0;
          sa_othr11 = 0;
          sa_#pos11 = 0;
          sa_dolr12 = 0;
          sa_wght12 = 0;
          sa_volm12 = 0;
          sa_puri12 = 0;
          sa_othr12 = 0;
          sa_#pos12 = 0;
          if umonth = 01;
             sa_dolr01 = so_actunet;
             sa_wght01 = so_actuwgt;
             sa_volm01 = so_actuvol;
             sa_puri01 = so_actupqt;
             sa_othr01 = so_actuoth;
             sa_#pos01 = 1;
          endif;

          if umonth = 02;
             sa_dolr02 =  so_actunet;
             sa_wght02 =  so_actuwgt;
             sa_volm02 =  so_actuvol;
             sa_puri02 =  so_actupqt;
             sa_othr02 =  so_actuoth;
             sa_#pos02 =  1;
          endif;

          if umonth = 03;
             sa_dolr03 =  so_actunet;
             sa_wght03 =  so_actuwgt;
             sa_volm03 =  so_actuvol;
             sa_puri03 =  so_actupqt;
             sa_othr03 =  so_actuoth;
             sa_#pos03 =  1;
          endif;

          if umonth = 04;
             sa_dolr04 =  so_actunet;
             sa_wght04 =  so_actuwgt;
             sa_volm04 =  so_actuvol;
             sa_puri04 =  so_actupqt;
             sa_othr04 =  so_actuoth;
             sa_#pos04 =  1;
          endif;

          if umonth = 05;
             sa_dolr05 =  so_actunet;
             sa_wght05 =  so_actuwgt;
             sa_volm05 =  so_actuvol;
             sa_puri05 =  so_actupqt;
             sa_othr05 =  so_actuoth;
             sa_#pos05 =  1;
          endif;

          if umonth = 06;
             sa_dolr06 =  so_actunet;
             sa_wght06 =  so_actuwgt;
             sa_volm06 =  so_actuvol;
             sa_puri06 =  so_actupqt;
             sa_othr06 =  so_actuoth;
             sa_#pos06 =  1;
          endif;

          if umonth = 07;
             sa_dolr07 =  so_actunet;
             sa_wght07 =  so_actuwgt;
             sa_volm07 =  so_actuvol;
             sa_puri07 =  so_actupqt;
             sa_othr07 =  so_actuoth;
             sa_#pos07 =  1;
          endif;

          if umonth = 08;
             sa_dolr08 =  so_actunet;
             sa_wght08 =  so_actuwgt;
             sa_volm08 =  so_actuvol;
             sa_puri08 =  so_actupqt;
             sa_othr08 =  so_actuoth;
             sa_#pos08 =  1;
          endif;

          if umonth = 09;
             sa_dolr09 =  so_actunet;
             sa_wght09 =  so_actuwgt;
             sa_volm09 =  so_actuvol;
             sa_puri09 =  so_actupqt;
             sa_othr09 =  so_actuoth;
             sa_#pos09 =  1;
          endif;

          if umonth = 10;
             sa_dolr10 =  so_actunet;
             sa_wght10 =  so_actuwgt;
             sa_volm10 =  so_actuvol;
             sa_puri10 =  so_actupqt;
             sa_othr10 =  so_actuoth;
             sa_#pos10 =  1;
          endif;

          if umonth = 11;
             sa_dolr11 =  so_actunet;
             sa_wght11 =  so_actuwgt;
             sa_volm11 =  so_actuvol;
             sa_puri11 =  so_actupqt;
             sa_othr11 =  so_actuoth;
             sa_#pos11 =  1;
          endif;

          if umonth = 12;
           sa_dolr12 =  so_actunet;
           sa_wght12 =  so_actuwgt;
           sa_volm12 =  so_actuvol;
           sa_puri12 =  so_actupqt;
           sa_othr12 =  so_actuoth;
           sa_#pos12 =  1;
        endif;
       endif;

       //supplier record not found, add it
       if not %found(k_suplpurb);
          sa_birth = lda_sysdat;
          write rk_suplpur;
       else;
          update rk_suplpur;
       endif;

       endsr;

       /////////////////////////////////////////////////////////////////////

       /////////////////////////////////////////////////////////////////////

       //--------------------------------- Message logic for screen programs
       //*copy k3s_c051
       //***************************************************************
       //**
       //**   K3S-Replenish - Inventory REPLENISHment System
       //**   Copyright (c) 1996-1997 by King III Solutions, Inc.
       //**   All rights reserved.
       //**
       //*****************************************************************
       //**
       //**   Name: K3S_C051
       //**   Type: ILE /COPY member
       //**   Desc: Message logic for screen programs 'C' specifications
       //**
       //*****************************************************************

       //////////////////////////////////////////////// Add message to array

       begsr $_add_msg;
       if @mn < 25;
          @mn += 1;

          @msgid(@mn)   = @msg_id;
          @msgtext(@mn) = @msg_text;

          *in99 = *on;

          clear @msg_id;
          clear @msg_text;

       endif;
       endsr;

       ///////////////////////////////////////////// Process subfile message

       begsr $_pass_msg;

       //call module to process subfile messages
       callp K3S_MSGCL(@msgfile:
                      @msgid:
                      @msgtext);

       clear @msgid;
       clear @msgtext;
       clear @mn;

       endsr;

       ///////////////////// Place num_record into @msg_text and strip zeros

       begsr $_num_recd;

       @records = %editc(@num_recds:'X');

       if @records1 = '0';
          @records1 = %xlate('0':' ':@records1);

          if @records2 = '0';
             @records2 = %xlate('0':' ':@records2);

             if @records3 = '0';
                @records3 = %xlate('0':' ':@records3);

                if @records4 = '0';
                   @records4 = %xlate('0':' ':@records4);

                endif;
             endif;
          endif;
       endif;

       @msg_text = @records;

       endsr;


       //----------------------------------------Cursor posistion subroutine
       //*copy k3s_c080
       //*****************************************************************
       //**
       //**   K3S-Replenish - Inventory REPLENISHment System
       //**   Copyright (c) 1996-1997 by King III Solutions, Inc.
       //**   All rights reserved.
       //**
       //*****************************************************************
       //**
       //**   Name: K3S_C080
       //**   Type: ILE /COPY member
       //**   Desc: Find cursor position on screen
       //**
       //**************************************************************

       /////////////////////////////////////////////// find cursor posistion

       begsr $_cur_pos;

       pos_row = pos_cursor / 256;
       pos_column = %rem(pos_cursor:256);

       endsr;


       //----------------------------------------$_get_note subroutine
       //c*copy k3s_c151
       //*****************************************************************
       //**
       //**   K3S-Replenish - Inventory REPLENISHment System
       //**  Copyright (c) 1996-1997 by King III Solutions, Inc.
       //**   All rights reserved.
       //**
       //*****************************************************************
       //**
       //**   Name: K3S_C151
       //**   Type: ILE /COPY member
       //**   Desc: Notes system - $_get_note subroutine
       //**
       //*****************************************************************

       //////////////////////////////////////////////// Get notes

       begsr $_get_note;

       //prime key list for notes
       //first get any reminder type notes
       *in84 = *off;
       @n_typnote = 'R';
       zz_noteflg = *blanks;
       zz_note = *blanks;
       @n_contx = *zeros;
       @n_notex = *blanks;
       notepada_key.nt_comp = so_comp;
       notepada_key.nt_locn = so_locn;
       notepada_key.nt_typval = @n_typval;
       notepada_key.nt_supl = so_supl;
       notepada_key.nt_suplsub = so_suplsub;
       notepada_key.nt_prod = @n_prod;
       notepada_key.nt_deal = @n_deal;
       notepada_key.nt_seasonl = @n_seasonl;
       notepada_key.nt_typnote = @n_typnote;
       setll %kds(notepada_key:9) k_notepada;
       reade(n) %kds(notepada_key:9) k_notepada;

       dow not %eof(k_notepada);
          @n_okx = '0';
          callp K3S_M100(nt_expire:
                        lda_usrdat:
                        not_six:
                        char_date);

          if char_date = *blanks;
             @n_okx = '1';
          endif;
          if (char_date <> *blanks) and
             (nt_expire > lda_sysdat);
             @n_okx = '1';
          endif;

          if nt_locn = *blanks;
             @n_okx = '0';
          endif;

          if (@n_typval = 'U') and
             (nt_user <> lda_userid);
             @n_okx = '0';
          endif;

          if @n_okx = '1';
             @n_contx += 1;
          endif;

          if (zz_note = *blanks) and
             (nt_remind = lda_sysdat) and
             (@n_okx = '1');
             *in84 = *on;
             @n_birth = %char(nt_birth:*iso);
             zz_note   = nt_note;
             @n_compx   = nt_comp;
             @n_locnx   = nt_locn;
             @n_typvalx = nt_typval;
             @n_suplx   = nt_supl;
             @n_suplsux  = nt_suplsub;
             @n_prodx   = nt_prod;
             @n_dealx   = nt_deal;
             @n_notex   = nt_note;
             @n_seasonx = nt_seasonl;
             @n_typnotx = nt_typnote;
             @n_sequenx = nt_sequenc;
          endif;
          if @n_contx > 1;
             zz_noteflg = %replace('+':zz_noteflg:2:1);
          endif;
          reade(n) %kds(notepada_key:9) k_notepada;
       enddo;

       //now get any global notes
       @n_locnxx = *hival;

       notepada_key.nt_comp = so_comp;
       notepada_key.nt_locn = @n_locnxx;
       notepada_key.nt_typval = @n_typval;
       notepada_key.nt_supl = so_supl;
       notepada_key.nt_suplsub = so_suplsub;
       notepada_key.nt_prod = @n_prod;
       notepada_key.nt_deal = @n_deal;
       notepada_key.nt_seasonl = @n_seasonl;
       notepada_key.nt_typnote = @n_typnote;
       setll %kds(notepada_key:9) k_notepada;
       @n_locnxx = *blanks;
       notepada_key.nt_locn = @n_locnxx;
       setll %kds(notepada_key:9) k_notepada;
       reade(n) %kds(notepada_key:9) k_notepada;
       dow not %eof(k_notepada);

          @n_okx = '0';
          callp  K3S_M100(nt_expire:
                         lda_usrdat:
                         not_six:
                         char_date);
          if char_date = *blanks;
             @n_okx = '1';
          endif;
          if (char_date <> *blanks) and
             (nt_expire > lda_sysdat);
             @n_okx = '1';
          endif;

          if (@n_typval = 'U') and
             (nt_user <> lda_userid);
             @n_okx = '0';
          endif;

          if @n_okx = '1';
             @n_contx += 1;
          endif;

          if (zz_note = *blanks) and
             (nt_remind = lda_sysdat) and
             (@n_okx = '1');
             *in84 = *on;
             zz_note   = nt_note;
             @n_birth = %char(nt_birth:*iso);
             @n_notex   = nt_note;
             @n_compx   = nt_comp;
             @n_locnx   = nt_locn;
             @n_typvalx = nt_typval;
             @n_suplx   = nt_supl;
             @n_suplsux  = nt_suplsub;
             @n_prodx   = nt_prod;
             @n_dealx   = nt_deal;
             @n_seasonx = nt_seasonl;
             @n_typnotx = nt_typnote;
             @n_sequenx = nt_sequenc;
             zz_noteflg = %replace('*':zz_noteflg:1:1);
         endif;
         if @n_contx > 1;
             zz_noteflg = %replace('*':zz_noteflg:1:1);
             zz_noteflg = %replace('+':zz_noteflg:2:1);
         endif;
         reade(n) %kds(notepada_key:9) k_notepada;
       enddo;

       //now get any notes for selected type (supplier/product etc.)
       @n_typnote = 'N';
       notepada_key.nt_typnote = @n_typnote;
       setll %kds(notepada_key:9) k_notepada;
       reade(n) %kds(notepada_key:9) k_notepada;

       dow not %eof(k_notepada);
          @n_okx = '0';
          callp K3S_M100(nt_expire:
                        lda_usrdat:
                        not_six:
                        char_date);
          if char_date = *blanks;
             @n_okx = '1';
          endif;
          if (char_date <> *blanks) and
             (nt_expire > lda_sysdat);
             @n_okx = '1';
          endif;

          if nt_locn = *blanks;
             @n_okx = '0';
          endif;

          if (@n_typval = 'U') and
             (nt_user <> lda_userid);
             @n_okx = '0';
          endif;

          if @n_okx = '1';
             @n_contx += 1;
          endif;

          if (zz_note = *blanks) and
             (@n_okx = '1');
             *in84 = *off;
             zz_note   = nt_note;
             @n_birth = %char(nt_birth:*iso);
             @n_compx   = nt_comp;
             @n_locnx   = nt_locn;
             @n_typvalx = nt_typval;
             @n_suplx   = nt_supl;
             @n_suplsux  = nt_suplsub;
             @n_prodx   = nt_prod;
             @n_dealx   = nt_deal;
             @n_notex   = nt_note;
             @n_seasonx = nt_seasonl;
             @n_typnotx = nt_typnote;
             @n_sequenx = nt_sequenc;
          endif;
          if @n_contx > 1;
             zz_noteflg = %replace('+':zz_noteflg:2:1);
          endif;
          reade(n) %kds(notepada_key:9) k_notepada;
       enddo;

       @n_locnxx = *hival;
       notepada_key.nt_locn = @n_locnxx;

       setll %kds(notepada_key:9) k_notepada;
       @n_locnxx = *blanks;
       notepada_key.nt_locn = @n_locnxx;
       setll %kds(notepada_key:9) k_notepada;
       reade(n) %kds(notepada_key:9) k_notepada;

       dow not %eof(k_notepada);
          @n_okx = '0';
          callp K3S_M100(nt_expire:
                        lda_usrdat:
                        not_six:
                        char_date);
          if char_date = *blanks;
             @n_okx = '1';
          endif;
          if (char_date <> *blanks) and
             (nt_expire > lda_sysdat);
              @n_okx = '1';
          endif;

          if (@n_typval = 'U') and
             (nt_user <> lda_userid);
             @n_okx = '0';
          endif;

          if @n_okx = '1';
             @n_contx += 1;
          endif;

          if (zz_note = *blanks) and
             (@n_okx = '1');
             *in84 = *off;
             zz_note   = nt_note;
             @n_birth = %char(nt_birth:*iso);
             @n_notex   = nt_note;
             @n_compx   = nt_comp;
             @n_locnx   = nt_locn;
             @n_typvalx = nt_typval;
             @n_suplx   = nt_supl;
             @n_suplsux  = nt_suplsub;
             @n_prodx   = nt_prod;
             @n_dealx   = nt_deal;
             @n_seasonx = nt_seasonl;
             @n_typnotx = nt_typnote;
             @n_sequenx = nt_sequenc;
             zz_noteflg = %replace('*':zz_noteflg:1:1);
          endif;
          if @n_contx > 1;
             zz_noteflg = %replace('*':zz_noteflg:1:1);
             zz_noteflg = %replace('+':zz_noteflg:2:1);
          endif;
          reade(n) %kds(notepada_key:9) k_notepada;
        enddo;

        endsr;

       //-----------------------------------------$_chg_note subroutine
       //c*copy k3s_c152
       //*****************************************************************
       //**
       //**   K3S-Replenish - Inventory REPLENISHment System
       //**   Copyright (c) 1996-1997 by King III Solutions, Inc.
       //**   All rights reserved.
       // **
       //*****************************************************************
       //**
       //**   Name: K3S_C152
       //**   Type: ILE /COPY member
       //**   Desc: Notes system - $_chg_note subroutine
       //*
       //*****************************************************************

       //* /////////////////////////////////////////////// Change notes

       begsr $_chg_note;

       @n_notecx = '0';
       nt_birth = %date(@n_birth:*iso);
       //prime key list for notes
       if @n_notex <> *blanks;

          if (zz_note = *blanks) and
             (zz_noteflg = ' +') or
             (zz_note = *blanks) and
             (zz_noteflg = '  ');
             notepada_key.nt_comp = @n_compx;
             notepada_key.nt_locn = @n_locnx;
             notepada_key.nt_typval = @n_typvalx;
             notepada_key.nt_supl = @n_suplx;
             notepada_key.nt_suplsub = @n_suplsux;
             notepada_key.nt_prod = @n_prodx;
             notepada_key.nt_deal = @n_dealx;
             notepada_key.nt_seasonl = @n_seasonx;
             notepada_key.nt_typnote = @n_typnotx;
             notepada_key.nt_birth = nt_birth;
             notepada_key.nt_sequenc = @n_sequenx;
             chain %kds(notepada_key) k_notepada;
             if %found(k_notepada);
                @n_notecx = '1';
                @n_notex = *blanks;
                delete rk_notepad;
             endif;
          endif;

          if (zz_note <> *blanks) and
             (zz_note <> @n_notex) and
             (zz_noteflg = ' +') or
             (zz_note <> *blanks) and
             (zz_note <> @n_notex) and
             (zz_noteflg = '  ');
             chain %kds(notepada_key) k_notepada;
             if %found(k_notepada);
                nt_note = zz_note;
                @n_notex  = zz_note;
                @n_notecx = '1';
                nt_lastupd = lda_sysdat;
                update rk_notepad;
             endif;
          endif;

          if (zz_note <> *blanks) and
             (zz_note <> @n_notex) and
             (zz_noteflg = '*+') or
             (zz_note <> *blanks) and
             (zz_note <> @n_notex) and
             (zz_noteflg = '* ');
             @n_locnx = @n_locn;
             @n_sequenx = 10;
             dou %eof(k_notepada);
                 nt_birth   = lda_sysdat;
                 @n_birth = %char(lda_sysdat:*iso);
                 if zz_program <> 'K3S_5050' and
                    zz_program <> 'K3S_5051';
                    nt_birth   = lda_sysdat;
                 else;
                    nt_birth = %date('0001-01-01':*iso);
                 endif;
                 notepada_key.nt_comp = @n_compx;
                 notepada_key.nt_locn = @n_locnx;
                 notepada_key.nt_typval = @n_typvalx;
                 notepada_key.nt_supl = @n_suplx;
                 notepada_key.nt_suplsub = @n_suplsux;
                 notepada_key.nt_prod = @n_prodx;
                 notepada_key.nt_deal = @n_dealx;
                 notepada_key.nt_seasonl = @n_seasonx;
                 notepada_key.nt_typnote = @n_typnotx;
                 notepada_key.nt_birth  = nt_birth;
                 notepada_key.nt_sequenc = @n_sequenx;
                 chain %kds(notepada_key) k_notepada;
                 if not %found(k_notepada);
                    nt_comp   = @n_compx;
                    nt_locn   = @n_locnx;
                    nt_typval = @n_typvalx;
                    nt_supl   = @n_suplx;
                    nt_suplsub = @n_suplsux;
                    nt_prod   = @n_prodx;
                    nt_deal   = @n_dealx;
                    nt_note   = zz_note;
                    nt_seasonl = @n_seasonx;
                    nt_typnote = @n_typnotx;
                    nt_sequenc = @n_sequenx;
                    @n_notecx = '1';
                    nt_lastupd = lda_sysdat;
                    if zz_program <> 'K3S_5050' and
                       zz_program <> 'K3S_5051';
                       nt_birth   = lda_sysdat;
                    else;
                       nt_birth = %date('0001-01-01':*iso);
                    endif;
                    nt_user = lda_userid;
                    nt_expire = %date('0001-01-01':*iso);
                    nt_remind = %date('0001-01-01':*iso);
                    write rk_notepad;
                 endif;
                 @n_sequenx = @n_sequenx + 10;
             enddo;
          endif;

       endif;

       if (@n_notex = *blanks) and
          (zz_note <> *blanks) and
          (zz_noteflg = '  ');
          @n_compx   = @n_comp;
          @n_locnx   = @n_locn;
          @n_typvalx = @n_typval;
          @n_suplx   = @n_supl;
          @n_suplsux  = @n_suplsub;
          @n_prodx   = @n_prod;
          @n_dealx   = @n_deal;
          @n_notex   = zz_note;
          @n_seasonx = @n_seasonl;
          @n_typnotx = 'N';
          @n_sequenx = 10;

          dou not %found(k_notepada);
             nt_birth   = lda_sysdat;
             @n_birth = %char(lda_sysdat:*iso);
             if  zz_program <> 'K3S_5050' and
                 zz_program <> 'K3S_5051';
                 nt_birth   = lda_sysdat;
             else;
                 nt_birth = %date('0001-01-01':*iso);
             endif;
             notepada_key.nt_comp = @n_compx;
             notepada_key.nt_locn = @n_locnx;
             notepada_key.nt_typval = @n_typvalx;
             notepada_key.nt_supl = @n_suplx;
             notepada_key.nt_suplsub = @n_suplsux;
             notepada_key.nt_prod = @n_prodx;
             notepada_key.nt_deal = @n_dealx;
             notepada_key.nt_seasonl = @n_seasonx;
             notepada_key.nt_typnote = @n_typnotx;
             notepada_key.nt_birth  = nt_birth;
             notepada_key.nt_sequenc = @n_sequenx;
             chain %kds(notepada_key) k_notepada;
             if not %found(k_notepada);
                nt_comp   = @n_compx;
                nt_locn   = @n_locnx;
                nt_typval = @n_typvalx;
                nt_supl   = @n_suplx;
                nt_suplsub = @n_suplsux;
                nt_prod   = @n_prodx;
                nt_deal   = @n_dealx;
                nt_note   = zz_note;
                nt_seasonl = @n_seasonx;
                nt_typnote = @n_typnotx;
                nt_sequenc = @n_sequenx;
                @n_notecx = '1';
                nt_lastupd = lda_sysdat;
                if zz_program <> 'K3S_5050' and
                   zz_program <> 'K3S_5051';
                   nt_birth   = lda_sysdat;
                else;
                   nt_birth   = %date('0001-01-01':*iso);
                endif;
                nt_user    = lda_userid;
                nt_expire  = %date('0001-01-01':*iso);
                nt_remind  = %date('0001-01-01':*iso);
                write rk_notepad;
             endif;
             @n_sequenx = @n_sequenx + 10;
          enddo;
       endif;

       endsr;


       //----------------------------------------$_get_lda  subroutine
       //c*copy k3s_c031
       //*****************************************************************
       //**
       //  K3S-Replenish - Inventory REPLENISHment System
       //  Copyright (c) 1996-1997 by King III Solutions, Inc.
       //  All rights reserved.
       //**
       //*****************************************************************
       //**
       //**   Name: K3S_C031
       //**   Type: ILE /COPY member
       //**   Desc: *LDA Local data area - get LDA data
       //**
       //**************************************************************

       ////////////////////////////////////////////////// Get data area *lda

       begsr $_get_lda;

       //retrieve local data area *lda
       in *dtaara;

       //if K3S-Replenish date not equal AS/400 system date, set on *in95
       //  to display date with underline attribute
       *in95 = (lda_sysdat <> lda_cmpdat);

       //if user prefered time adjustment being used, set on indicator 96
       //to display time with underline attribute
       *in96 = (lda_usradj <> 0);

       endsr;


       //c----------------------------------------$_get_time subroutine
       //c*copy k3s_c180
       //*****************************************************************
       //**
       //**   K3S-Replenish - Inventory REPLENISHment System
       //**   Copyright (c) 1996-1997 by King III Solutions, Inc.
       //**   All rights reserved.
       //**
       //*****************************************************************
       //**
       //**   Name: K3S_C180
       //**   Type: ILE /COPY member
       //**   Desc: Get system date and time formated       'C' specs only
       //**
       //*****************************************************************

       ////////////////////////////////////////// Get date and time formated

       begsr $_get_time;

       //-------------------------------------------- Retrieve date and time
       //call module to retrieve user formated date and time
       callp K3S_M120(lda_usrdat:
                     lda_usrtim:
                     lda_usradj:
                     zz_usrdate:
                     zz_usrtime);

       endsr;


       //*-----------------------------------------------$_get_titl subroutine
       // c*copy k3s_c200
       //*****************************************************************
       //**
       //**   K3S-Replenish (R) - Inventory REPLENISHment System
       //**   Copyright (C) 1996-2008 by King III Solutions, Inc.
       //**   Program property of King III Solutions, Inc.
       //**   All rights reserved.
       //**   K3S_Replenish (R) is a Registered Trade Mark of
       //**   King III Solutions Inc.
       //**
       //*****************************************************************
       //**   Desc: Get screen title                        'C' specs only
       //**
       //*****************************************************************

       ////////////////////////////////////////////////// Get screen title

       begsr $_get_titl;

       //get title of screen
       clear @blanks;
       ta_comp    = lda_comp;
       ta_codetyp = 'TLE';
       ta_codeval = %trimr(zz_program) + 'FM' +
                    @recd_frmt;

       //does title record exist?
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;

       //title record does exist
       if %found(k_tablcoda);

       //determine title length, and starting position for centering
          @length = %checkr(' ':ta_codeds1);
          @start     = (50 - @length) / 2;

       //   center title
          zz_title = %subst(@blanks:1:(@start)) +
                     ta_codeds1;

       //   title record does not exist, so provide message
       else;
          zz_title = @no_title;
       endif;

       endsr;
      /end-free

