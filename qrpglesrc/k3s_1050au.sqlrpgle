**************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)
     h OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
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


     fK3S_1050fmcf   e             workstn infds(infds)                         work station file
     f                                     usropn                               work station file

     f*_companyauf   e           k disk                                         buy groups
      * Company values by company id

     f*_trancenaif   e           k disk                                         buy groups
      * Transfer supplier file

     f*_supliera   auf   e           k disk                                         suppliers
      * suppliers by location, supplier, sub supplier

     fk_notepadauf a e           k disk                                         suppliers
      * note pad file

     f*_suplapvauf a e           k disk                                         suppliers
      * note pad file

     f*_buyrsotauf   e           k disk                                         buy groups
      * buy group suggested order totals

     f*_suplsoqauf a e           k disk                                         deal summary info
      * selected products batches

     f*k_suplpurbuf a e           k disk                                         deal summary info
      * supplier purchase history

     f*k_suplpodaif   e           k disk                                        deal summary info
      * supplier purchase history

     f*_tablcodaif   e           k disk                                         table codes
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

      * -------------------------------------------------- parameters passed
     d soqseq#x        s              5a                                        batch number
     d screenvmi       s              1a   inz(' ')                             location
     d openscrn        s              1a   inz(' ')                             location
     d vnd5            s              5s 0
     d workstatn       s             10a                                        location
     d savpo#          s                   like(so_po#)                         location
     d zz_narcot       s             10a                                        location
     d zz_codetyp      s              3                                         location
     d zz_codeval      s             20                                         location
     d zz_buyr         s                   like(so_buyr)                        location
     d zz_locn         s                   like(so_locn)                        location
     d xx_locnfrm      s                   like(so_locn)                        location
     d xx_locnto       s                   like(so_locn)                        location
     d zz_sub          s                   like(so_suplsub)                     location
     d zz_supl         s                   like(so_supl)                        location
     d zz_suplsub      s                   like(so_suplsub)                     location
     d zz_note         s                   like(nt_note)                        location
     d zz_noteflg      s              2                                         location
     d blank           s              1                                         location
     d flag3_test      s              1  0                                      location
     d byname          s             35                                         location
     d lcdesc          s             40                                         location
     d table_cnt       s              1  0                                      location
     d suplapv_cnt     s              1  0                                      location
     d tacodeds1       s            100                                         location
     d spname          s             40                                         location
     d spleadtmo       s              3  0                                      location
     d splocn          s              5                                         location
     d spsupl          s             10                                         location
     d spsuplsub       s             10                                         location

      * --------------------------------------------------------- Workfields
     d save_locn       s              5                                         save location id
     d hold_locn       s              5                                         save location id
     d save_supl       s                   like(sp_supl)                        save supplier id
     d save_sub        s                   like(sp_suplsub)                     save sub supplier id
     d save_buyr       s                   like(so_buyr)                        save buy group id
     d fxc_data        s              1                                         errors detected ?
     d vfaxcd          s              1                                         errors detected ?
     d zz_skiplt       s              1                                         make alpha per K3
     d*zz_skiplt       s              1  0                                      errors detected ?
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
     d xx_year         s              4  0                                      batch begin date
      * ---------------------------------------------------
     d
     d suplapv_rec   e ds                  ExtName(k_suplapv)
     d suplier_rec   e ds                  ExtName(k_suplier)
     d buyrsot_rec   e ds                  ExtName(k_buyrsot)
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
     d suplpur_rec   e ds                  ExtName(k_suplpur)
      * ---------------------------------------------------
     d/copy k3s_proto
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
     d  @updated                      1  0
     d  @returned                     3  0
     d*
      * -------------------------------------- Likerec Statements
     d notepada_key    DS                  likerec(rk_notepad:*key)
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * ---------------------------------------------------------- Key Lists
     d comp_ds         DS
     d  cmcompcod                     3
     d  cmsysdate                      d
     d  cmapprove                     1  0
      * -------------------------------------------------------
     d suplsoq_ds      DS
     d  sosoqtype                     2
     d  soaltsrce                     1  0
      * ---------------------------------------------------------- Key Lists
     d soqactu_ds      DS
     d  soactunet                    13  4
     d  soactuwgt                    13  3
     d  soactuvol                    13  3
     d  soactupqt                    13  4
     d  soactuoth                    13  4
      * -------------------------------------------------------
     d suplapv_ds      DS
     d  svcoment1                    25
     d  svcoment2                    25
     d  svcoment3                    25
     d  svcoment4                    25
     d  svcoment5                    25
     d  svcoment6                    25
     d  svcoment7                    25
      * -------------------------------------------------------
     d trancen_ds      DS
     d  tclocnto                      5
     d  tclocnfrm                     5
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;
       //once routine
       exsr $_once;

       //set off error
       errors = *off;

       zz_pomsg1 = spsupl + ' ' + spname;
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
          exsr dclsvcursor;
          exsr opnsvcursor;
          If SQLState = SQLStateOk;
             exec sql
               fetch next from svcursor
                 into :suplapv_rec;
             if SQLState = SQLStateOk;
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
                exsr updsuplapv;
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
               sv_supl    = spsupl;
               sv_suplsub = spsuplsub;
               exsr insert_suplapv;
             endif;
             exsr clssvcursor;
          endif;
          exsr dclsocursor;
          exsr opnsocursor;
          If SQLState = SQLStateOk;
             exec sql
               fetch next from socursor
                 into :suplsoq_rec;
             if SQLState = SQLStateOk;
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
                   exsr dclbtcursor;
                   exsr opnbtcursor;
                   If SQLState = SQLStateOk;
                      exec sql
                        fetch next from btcursor
                        into :buyrsot_rec;
                        if SQLState = SQLStateOk;
                           select;
       //Fixed cycle
                             when so_fxcfrq > 0;
                                bt_duefc#r -= 1;
                                bt_duefcvr -= so_actunet;
                                bt_aprov# += 1;
                                bt_aprovv += so_actunet;
                                exsr updbuyrsot;
       //Reminder
                             when so_chkremi > 0;
                                bt_duerm#r -= 1;
                                bt_duermvr -= so_actunet;
                                bt_aprov# += 1;
                                bt_aprovv += so_actunet;
                                exsr updbuyrsot;
       //Forward buy
                             when so_chkfbuy > 0;
                                bt_duefb#r -= 1;
                                bt_duefbvr -= so_actunet;
                                bt_aprov# += 1;
                                bt_aprovv += so_actunet;
                                exsr updbuyrsot;
       //No Delay
                             when so_joint = 0;
                                bt_duend#r -= 1;
                                bt_duendvr -= so_actunet;
                                bt_aprov# += 1;
                                bt_aprovv += so_actunet;
                                exsr updbuyrsot;

                             other;
       //Not due
                                bt_notdu#r -= 1;
                                bt_notduvr -= so_actunet;
                                bt_aprov# += 1;
                                bt_aprovv += so_actunet;
                                exsr updbuyrsot;
                           endsl;
                        endif;
                        exsr clsbtcursor;
                   endif;
                   so_soqtype = 'AP';
       // zz_faxpo = ' ';
       //                if        sppfaxord = 1
       //                eval      vfaxcd = 'Y'
       //                else
       //                eval      vfaxcd = ' '
       //                endif
                endif;
                so_po# = savpo#;                                                  //note type value
                so_ordate = lda_cmpdat;
                so_expdelv = xx_arvdate;
                exsr updsuplsoq;                                                 //location id
             endif;
             exsr clssocursor;                                                   //location id
          endif;
       // create po interface records header & detail

       // get supplier record
          exsr dclspcursor;
          exsr opnspcursor;
          If SQLState = SQLStateOk;
             exec sql
               fetch next from spcursor
               into :suplier_rec;
             If SQLState = SQLStateOk;
       //---------------------------------------------------------------
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
                      sp_fxcnxt = lda_cmpdat + %days(42);
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
                exsr updsuplier;
             endif;
             exsr clsspcursor;
          endif;
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
          //               zz_criticl:
          //               zz_loadcod:
                           zz_skiplt);
             if cmapprove = 0 and
                so_suplsub <> 'h';
                exsr $_add_pur;
             endif;
             if cmapprove = 0 and
                so_suplsub = 'h';
                exsr $_add_purc;
             endif;
          endif;
          finished = *on;
       endif;

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
       //zz_criticl = 0;                                                        //Cmt out per K3
       //zz_loadcod = ' ';                                                      //Cmt our per K3
       zz_skiplt = '0';                                                         //Make alpha per k3
       //zz_skiplt = 0;                                                         //Cmt out per K3
       @n_typval = 'H';
       @n_locn = locn;                                                          //location id
       @n_supl = supl;                                                          //supplier id
       @n_suplsub = suplsub;                                                    //sub supplier id
       @n_suplz = *blanks;                                                      //supplier id
       @n_suplsuz = *blanks;                                                    //sub supplier id

       if (mode = ' delete ') or
          (mode = '  view  ');
              *in58 = *on;
              *in30 = *off;
              zz_hldreas = *blanks;
              zz_begin   = *blanks;
              zz_end     = *blanks;
              *in59 = *off;
              onetime = '1';
       endif;
       if mode = '  add   ';
          onetime = ' ';
          *in59 = *on;
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
       save_locn = locn;
       zz_locn   = locn;

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
       exec sql
         select cm_compcod, cm_sysdate, cm_approve
           into :comp_ds
           from k_company
           where cm_comp = :lda_comp
           fetch first row only;

       if SQLState = SQLStateOk;
          callp K3S_7000CL(lda_comp:
                          zz_ponum);
          savpo# = zz_ponum;
       endif;

       //prime company code
       zz_compcod = cmcompcod;

       //get batch header record
       in *dtaara;
       lda_comp = comp;
       lda_cmpdat = cmsysdate;
       lda_sysdat = cmsysdate;
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

       exec sql
         select so_soqtype, so_altsrce
           into :suplsoq_ds
           from k_suplsoq
           where so_comp = :lda_comp and
                 so_buyr = :buyr and
                 so_locn = :locn and
                 so_supl = :supl and
                 so_suplsub = :suplsub and
                 so_soqseq# = :soqseq#
           fetch first row only;

       xx_date = %char(lda_cmpdat:*iso);
       sa_year = %int(%dec(%subst(xx_date:1:4):4:0));

       if SQLState = SQLStateOk;
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
                    xx_arvdate = lda_cmpdat + %days(spleadtmo);
                 endif;
                 exec sql
                   select tc_locnto, tc_locnfrm
                      into :trancen_ds
                      from k_trancen
                      where tc_comp = :lda_comp and
                            tc_locnto = :splocn and
                            tc_supl = :spsupl and
                            tc_suplsub = :spsuplsub
                      fetch first row only;
                 if SQLState = SQLStateOk;
                    *in30 = *on;
                    zz_potype = 'T';
                    xx_locnto = tclocnto;
                    xx_locnfrm = tclocnfrm;
                 else;
                    zz_potype = 'R';
                    xx_locnto = *blanks;
                    xx_locnfrm = *blanks;
                 endif;
                 if (soaltsrce = 1) and
                    (sosoqtype <> 'PN');
                        *in30 = *on;
                 endif;
       //   get po number
                 if (errors = *off) and
                    (sosoqtype <> 'PN') and
                    (sopo#  = *blanks);
                        zz_ponum = savpo#;
                        if *in30 = *off;
                           callp K3S_7010CL(lda_comp:
                                           locn:
                                           supl:
                                           suplsub:
                                           zz_potype);
                         endif;
                        if (soaltsrce = 1) and
                           (sosoqtype <> 'PN');
                               zz_potype = 'P';
                        endif;
                 else;
                     if *in30 = *off;
                        callp K3S_7010CL(lda_comp:
                                        locn:
                                        supl:
                                        suplsub:
                                        zz_potype);
                     endif;
                     if (soaltsrce = 1) and
                        (sosoqtype <> 'PN');
                           zz_potype = 'P';
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
       sopo# = savpo#;                                                         //note type value
       @returned = *zeros;
       exec sql
         select sv_coment1, sv_coment2, sv_coment3, sv_coment4, sv_coment5,
                sv_coment6, sv_coment7
           into :suplapv_ds
           from k_suplapv
           where sv_comp = :lda_comp and
                 sv_supl = :sp_supl and
                 sv_suplsub = :sp_suplsub
           fetch first row only;
       if SQLState = SQLStateOk;
          zz_pomsg1o = svcoment1;
          zz_pomsg2o = svcoment2;
          zz_pomsg3o = svcoment3;
          zz_pomsg4o = svcoment4;
          zz_pomsg5o = svcoment5;
          zz_pomsg6o = svcoment6;
          zz_pomsg7o = svcoment7;
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

       //blank supplier would mean all suppliers
       if supl   = *blanks;
          save_supl = supl;

       //otherwise, validate entry
       else;

       //get supplier record
          exec sql
            select sp_name, sp_supl, sp_suplsub
              into :spname, :spsupl, :spsuplsub
              from k_suplier
              where sp_comp = :comp and
                    sp_locn = :locn and
                    sp_supl = :supl and
                    sp_suplsub = :suplsub
              fetch first row only;

       // supplier row not found, send back error
          if SQLState = RowNotFound;
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
                             spname;
             else;
                zz_suplnam = %trimr(zz_supl) + ' ' +
                             zz_suplsub + ' ' +
                             spname;
             endif;
       //----------------------

          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////////// Edit supplier

       begsr $_edt_supc;

       *in35 = *off;

       if so_suplsub = 'h';
          *in35 = *on;
       // prime key list for supplier
          //sp_comp    = lda_comp;
          //sp_locn    = so_cmblocn;
          //sp_supl    = so_cmbsupl;
          //sp_suplsub = so_cmbsub;
       // get supplier record
          exec sql
            select sp_name
              into :spname
              from k_suplier
              where sp_comp = :lda_comp and
                    sp_locn = :so_cmblocn and
                    sp_supl = :so_cmbsupl and
                    sp_suplsub = :so_cmbsub
              fetch first row only;

       // supplier row not found, send back error
          if SQLState = RowNotFound;
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
                              spname;
              else;
                 zz_suplcmb = 'Loc' + ' ' +
                              so_cmblocn + ' ' +
                              %trimr(so_cmbsupl) + ' ' +
                              so_cmbsub + ' ' +
                              spname;
              endif;
       //----------------------

          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////////// Edit supplier

       begsr d_edt_supl;

       //blank supplier would mean all suppliers
       if supl   = *blanks;
          save_supl = supl;

       //otherwise, validate entry
       else;

       //get supplier record
          exec sql
            select sp_name, sp_leadtmo, sp_locn, sp_supl, sp_suplsub
              into :spname, :spleadtmo, :splocn, :spsupl, :spsuplsub
              from k_suplier
              where sp_comp = :lda_comp and
                    sp_locn = :locn and
                    sp_supl = :supl and
                    sp_suplsub = :suplsub
              fetch first row only;

       // supplier row not found, send back error
          if SQLState = RowNotFound;

       //supplier row not found, send back error
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
                             spname;
             else;
                zz_suplnam = %trimr(zz_supl) + ' ' +
                             zz_suplsub + ' ' +
                             spname;
             endif;
       //----------------------

          endif;

       endif;

       endsr;



       //////////////////////////////////////////////// Edit buy group entry

       begsr $_edt_buyr;

       //blank buy group would mean all buy groups
       if buyr = *blanks;
          save_buyr = buyr;

       //otherwise, validate entry
       else;
          exec sql
            select by_name
              into :byname
              from k_buyrgrp
              where by_comp = :lda_comp and
                    by_buyr = :buyr
              fetch first row only;

       //    buy group does not exist
          if SQLState = RowNotFound;
             errors    = *on;
             send_error= *on;
             *in51     = *on;
             @msg_id   = 'K3_4050';
             @msg_text = buyr;
             exsr $_add_msg;
          else;
             zz_buyrnam = %trimr(zz_buyr) + ' ' +
                          byname;
          endif;

       endif;

       endsr;

       ///////////////////////////////////////////////// Edit location entry

       begsr $_edt_locn;

       //blank location would mean all locations
       if locn   = *blanks;
          save_locn = locn;

       //otherwise, validate entry
       else;

       //get location record
          exec sql
            select lc_desc
              into :lcdesc
              from k_locatns
              where lc_comp = :lda_comp and
                    lc_locn = :locn
              fetch first row only;

       //    location does not exist
          if SQLState = RowNotFound;
              errors    = *on;
              send_error= *on;
              *in53     = *on;
              @msg_id   = 'K3_8020';
              @msg_text = locn;
              exsr $_add_msg;
           else;
              zz_locndes = %trimr(zz_locn) + ' ' +
                           lcdesc;
           endif;

       endif;

       endsr;

       //////////////////////////////////////////////// Edit buy group entry

       begsr d_edt_buyr;

       //blank buy group would mean all buy groups
       if buyr = *blanks;
          save_buyr = buyr;

       //otherwise, validate entry
       else;

       //get buy group record
          exec sql
            select by_name
              into :byname
              from k_buyrgrp
              where by_comp = :lda_comp and
                    by_buyr = :buyr
              fetch first row only;

       //    buy group does not exist
          if SQLState = RowNotFound;
             errors    = *on;
             send_error= *on;
             *in51     = *on;
             @msg_id   = 'K3_4050';
             @msg_text = buyr;
             exsr $_add_msg;
          else;
             zz_buyrnam = %trimr(zz_buyr) + ' ' +
                          byname;
          endif;

       endif;

       endsr;

       ///////////////////////////////////////////////// Edit location entry

       begsr d_edt_locn;

       //blank location would mean all locations
       if locn   = *blanks;
          save_locn = locn;

       //otherwise, validate entry
       else;

       //get location record
          exec sql
            select lc_desc
              into :lcdesc
              from k_locatns
              where lc_comp = :lda_comp and
                    lc_locn = :locn
              fetch first row only;

       //    location does not exist
          if SQLState = RowNotFound;
             errors    = *on;
             send_error= *on;
             *in53     = *on;
             @msg_id   = 'K3_8020';
             @msg_text = locn;
             exsr $_add_msg;
          else;
             zz_locndes = %trimr(zz_locn) + ' ' +
                          lcdesc;

          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////// Edit region entry

       begsr $_edt_type;

       //read this company only

       //get PO type record in table file
       exec sql
         select count(*)
           into :table_cnt
           from k_tablcod
           where ta_comp = :lda_comp and
                 ta_codetyp = 'POT' and
                 ta_codeval = :zz_potype
                 fetch first row only;

       //record not found
       if table_cnt = 0;
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
             *in58     = *on;
             @msg_id   = 'K3_9999';
             @msg_text = 'Begin & End must be entered    ';
             exsr $_add_msg;
       else;
          *in58 = *off;
       endif;

       if (zz_begin  <> *blanks) and
          (zz_end    <> *blanks) and
          (zz_hldreas =  *blanks);
             errors    = *on;
             *in61     = *on;
             @msg_id   = 'K3_9999';
             @msg_text = 'Hold reason must be entered    ';
             exsr $_add_msg;
       else;
             *in61     = *off;
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
          callp K3S_Date_To_ISO(zz_begin:
                               lda_usrdat:
                               lda_date6:
                               iso_begin:
                               iso_error);

       //requested begin date entered, not valid
          if iso_error = 1;
             errors    = *on;
             *in58     = *on;
             @msg_id   = 'K3_9030';
             @msg_text = zz_begin;
             exsr $_add_msg;
          else;
             *in58     = *off;
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
          callp K3S_Date_To_ISO(zz_end:
                                lda_usrdat:
                                lda_date6:
                                iso_end:
                                iso_error);

       //    requested end date entered, not valid
          if iso_error = 1;
             errors    = *on;
             *in59     = *on;
             @msg_id   = 'K3_9040';
             @msg_text = zz_end;
             exsr $_add_msg;
          else;
             *in59     = *off;
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
                *in58     = *on;
                @msg_id   = 'K3_9050';
                @msg_text = *blanks;
                exsr $_add_msg;
             else;
                *in58     = *off;
                xx_end   = iso_end;
                xx_begin   = iso_begin;
             endif;

          endif;

       endif;

       endsr;

       /////////////////////////////////////////////////////// Add purchase

       begsr $_add_pur;

       //prime key list for supplier
       xx_date = %char(lda_sysdat:*iso);
       xx_year = %dec(%subst(xx_date:1:4):4:0);

       exec sql
         select so_actunet, so_actuwgt, so_actuvol, so_actupqt, so_actuoth
            into soqactu_ds
            from k_suplsoq
            where so_comp = :lda_comp and
                  so_buyr = :buyr and
                  so_locn = :locn and
                  so_supl = :supl and
                  so_suplsub = :suplsub and
                  so_suplseq#= :soqseq#
            fetch first row only;

          if SQLState = SQLStateOk;
       //get supplier record
             exsr dclsacursor;
             exsr opnsacursor;
             if SQLState = SQLStateOk;
                exec sql
                  fetch next from sacursor
                     into :suplpur_rec;
                if SQLState = SQLStateOk;
                   sa_lastupd = lda_sysdat;

                   if umonth = 01;
                      sa_dolr01 += soactunet;
                      sa_wght01 += soactuwgt;
                      sa_volm01 += soactuvol;
                      sa_puri01 += soactupqt;
                      sa_othr01 += soactuoth;
                      sa_#pos01 += 1;
                   endif;

                   if umonth = 02;
                      sa_dolr02 += soactunet;
                      sa_wght02 += soactuwgt;
                      sa_volm02 += soactuvol;
                      sa_puri02 += soactupqt;
                      sa_othr02 += soactuoth;
                      sa_#pos02 += 1;
                   endif;

                   if umonth = 03;
                      sa_dolr03 += soactunet;
                      sa_wght03 += soactuwgt;
                      sa_volm03 += soactuvol;
                      sa_puri03 += soactupqt;
                      sa_othr03 += soactuoth;
                      sa_#pos03 += 1;
                   endif;

                   if umonth = 04;
                      sa_dolr04 += soactunet;
                      sa_wght04 += soactuwgt;
                      sa_volm04 += soactuvol;
                      sa_puri04 += soactupqt;
                      sa_othr04 += soactuoth;
                      sa_#pos04 += 1;
                   endif;

                   if umonth = 05;
                      sa_dolr05 += soactunet;
                      sa_wght05 += soactuwgt;
                      sa_volm05 += soactuvol;
                      sa_puri05 += soactupqt;
                      sa_othr05 += soactuoth;
                      sa_#pos05 += 1;
                   endif;

                   if umonth = 06;
                      sa_dolr06 += soactunet;
                      sa_wght06 += soactuwgt;
                      sa_volm06 += soactuvol;
                      sa_puri06 += soactupqt;
                      sa_othr06 += soactuoth;
                      sa_#pos06 += 1;
                   endif;

                  if umonth = 07;
                     sa_dolr07 += soactunet;
                     sa_wght07 += soactuwgt;
                     sa_volm07 += soactuvol;
                     sa_puri07 += soactupqt;
                     sa_othr07 += soactuoth;
                     sa_#pos07 += 1;
                  endif;

                  if umonth = 08;
                     sa_dolr08 += soactunet;
                     sa_wght08 += soactuwgt;
                     sa_volm08 += soactuvol;
                     sa_puri08 += soactupqt;
                     sa_othr08 += soactuoth;
                     sa_#pos08 += 1;
                  endif;

                  if umonth = 09;
                     sa_dolr09 += soactunet;
                     sa_wght09 += soactuwgt;
                     sa_volm09 += soactuvol;
                     sa_puri09 += soactupqt;
                     sa_othr09 += soactuoth;
                     sa_#pos09 += 1;
                  endif;

                  if umonth = 10;
                     sa_dolr10 += soactunet;
                     sa_wght10 += soactuwgt;
                     sa_volm10 += soactuvol;
                     sa_puri10 += soactupqt;
                     sa_othr10 += soactuoth;
                     sa_#pos10 += 1;
                  endif;

                  if umonth = 11;
                     sa_dolr11 += soactunet;
                     sa_wght11 += soactuwgt;
                     sa_volm11 += soactuvol;
                     sa_puri11 += soactupqt;
                     sa_othr11 += soactuoth;
                     sa_#pos11 += 1;
                  endif;

                  if umonth = 12;
                     sa_dolr12 += soactunet;
                     sa_wght12 += soactuwgt;
                     sa_volm12 += soactuvol;
                     sa_puri12 += soactupqt;
                     sa_othr12 += soactuoth;
                     sa_#pos12 += 1;
                  endif;
                  exsr updsuplpur;
                else;
                   sa_lastupd = lda_sysdat;
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
                      sa_dolr01 = soactunet;
                      sa_wght01 = soactuwgt;
                      sa_volm01 = soactuvol;
                      sa_puri01 = soactupqt;
                      sa_othr01 = soactuoth;
                      sa_#pos01 = 1;
                   endif;

                   if umonth = 02;
                      sa_dolr02 =  soactunet;
                      sa_wght02 =  soactuwgt;
                      sa_volm02 =  soactuvol;
                      sa_puri02 =  soactupqt;
                      sa_othr02 =  soactuoth;
                      sa_#pos02 =  1;
                   endif;

                   if umonth = 03;
                      sa_dolr03 =  soactunet;
                      sa_wght03 =  soactuwgt;
                      sa_volm03 =  soactuvol;
                      sa_puri03 =  soactupqt;
                      sa_othr03 =  soactuoth;
                      sa_#pos03 =  1;
                   endif;

                   if umonth = 04;
                      sa_dolr04 =  soactunet;
                      sa_wght04 =  soactuwgt;
                      sa_volm04 =  soactuvol;
                      sa_puri04 =  soactupqt;
                      sa_othr04 =  soactuoth;
                      sa_#pos04 =  1;
                   endif;

                   if umonth = 05;
                      sa_dolr05 =  soactunet;
                      sa_wght05 =  soactuwgt;
                      sa_volm05 =  soactuvol;
                      sa_puri05 =  soactupqt;
                      sa_othr05 =  soactuoth;
                      sa_#pos05 =  1;
                   endif;

                   if umonth = 06;
                      sa_dolr06 =  soactunet;
                      sa_wght06 =  soactuwgt;
                      sa_volm06 =  soactuvol;
                      sa_puri06 =  soactupqt;
                      sa_othr06 =  soactuoth;
                      sa_#pos06 =  1;
                  endif;

                  if umonth = 07;
                     sa_dolr07 =  soactunet;
                     sa_wght07 =  soactuwgt;
                     sa_volm07 =  soactuvol;
                     sa_puri07 =  soactupqt;
                     sa_othr07 =  soactuoth;
                     sa_#pos07 =  1;
                  endif;

                  if umonth = 08;
                     sa_dolr08 =  soactunet;
                     sa_wght08 =  soactuwgt;
                     sa_volm08 =  soactuvol;
                     sa_puri08 =  soactupqt;
                     sa_othr08 =  soactuoth;
                     sa_#pos08 =  1;
                  endif;

                  if umonth = 09;
                     sa_dolr09 =  soactunet;
                     sa_wght09 =  soactuwgt;
                     sa_volm09 =  soactuvol;
                     sa_puri09 =  soactupqt;
                     sa_othr09 =  soactuoth;
                     sa_#pos09 =  1;
                  endif;

                  if umonth = 10;
                     sa_dolr10 =  soactunet;
                     sa_wght10 =  soactuwgt;
                     sa_volm10 =  soactuvol;
                     sa_puri10 =  soactupqt;
                     sa_othr10 =  soactuoth;
                     sa_#pos10 =  1;
                  endif;

                  if umonth = 11;
                     sa_dolr11 =  soactunet;
                     sa_wght11 =  soactuwgt;
                     sa_volm11 =  soactuvol;
                     sa_puri11 =  soactupqt;
                     sa_othr11 =  soactuoth;
                     sa_#pos11 =  1;
                  endif;

                  if umonth = 12;
                     sa_dolr12 =  soactunet;
                     sa_wght12 =  soactuwgt;
                     sa_volm12 =  soactuvol;
                     sa_puri12 =  soactupqt;
                     sa_othr12 =  soactuoth;
                     sa_#pos12 =  1;
                  endif;
                  exsr insert_suplpur;
                endif;
                exsr clssacursor;
             endif;
          endif;

       endsr;

       /////////////////////////////////////////////////////// Add purchase

       begsr $_add_purc;

       //prime key list for supplier
       xx_date = %char(lda_sysdat:*iso);
       xx_year = %dec(%subst(xx_date:1:4):4:0);

       exec sql
         select so_actunet, so_actuwgt, so_actuvol, so_actupqt, so_actuoth
            into soqactu_ds
            from k_suplsoq
            where so_comp = :lda_comp and
                  so_buyr = :buyr and
                  so_locn = :locn and
                  so_supl = :supl and
                  so_suplsub = :suplsub and
                  so_suplseq#= :soqseq#
            fetch first row only;

          if SQLState = SQLStateOk;
       //get supplier record
             exsr dclsacursor;
             exsr opnsacursor;
             if SQLState = SQLStateOk;
                exec sql
                  fetch next from sacursor
                     into :suplpur_rec;
                 if SQLState = SQLStateOk;
                   sa_lastupd = lda_sysdat;

                   if umonth = 01;
                      sa_dolr01 += soactunet;
                      sa_wght01 += soactuwgt;
                      sa_volm01 += soactuvol;
                      sa_puri01 += soactupqt;
                      sa_othr01 += soactuoth;
                      sa_#pos01 += 1;
                   endif;

                   if umonth = 02;
                      sa_dolr02 += soactunet;
                      sa_wght02 += soactuwgt;
                      sa_volm02 += soactuvol;
                      sa_puri02 += soactupqt;
                      sa_othr02 += soactuoth;
                      sa_#pos02 += 1;
                   endif;

                   if umonth = 03;
                      sa_dolr03 += soactunet;
                      sa_wght03 += soactuwgt;
                      sa_volm03 += soactuvol;
                      sa_puri03 += soactupqt;
                      sa_othr03 += soactuoth;
                      sa_#pos03 += 1;
                   endif;

                   if umonth = 04;
                      sa_dolr04 += soactunet;
                      sa_wght04 += soactuwgt;
                      sa_volm04 += soactuvol;
                      sa_puri04 += soactupqt;
                      sa_othr04 += soactuoth;
                      sa_#pos04 += 1;
                   endif;

                   if umonth = 05;
                      sa_dolr05 += soactunet;
                      sa_wght05 += soactuwgt;
                      sa_volm05 += soactuvol;
                      sa_puri05 += soactupqt;
                      sa_othr05 += soactuoth;
                      sa_#pos05 += 1;
                   endif;

                   if umonth = 06;
                      sa_dolr06 += soactunet;
                      sa_wght06 += soactuwgt;
                      sa_volm06 += soactuvol;
                      sa_puri06 += soactupqt;
                      sa_othr06 += soactuoth;
                      sa_#pos06 += 1;
                   endif;

                   if umonth = 07;
                      sa_dolr07 += soactunet;
                      sa_wght07 += soactuwgt;
                      sa_volm07 += soactuvol;
                      sa_puri07 += soactupqt;
                      sa_othr07 += soactuoth;
                      sa_#pos07 += 1;
                   endif;

                   if umonth = 08;
                      sa_dolr08 += soactunet;
                      sa_wght08 += soactuwgt;
                      sa_volm08 += soactuvol;
                      sa_puri08 += soactupqt;
                      sa_othr08 += soactuoth;
                      sa_#pos08 += 1;
                   endif;

                   if umonth = 09;
                      sa_dolr09 += soactunet;
                      sa_wght09 += soactuwgt;
                      sa_volm09 += soactuvol;
                      sa_puri09 += soactupqt;
                      sa_othr09 += soactuoth;
                      sa_#pos09 += 1;
                   endif;

                   if umonth = 10;
                      sa_dolr10 += soactunet;
                      sa_wght10 += soactuwgt;
                      sa_volm10 += soactuvol;
                      sa_puri10 += soactupqt;
                      sa_othr10 += soactuoth;
                      sa_#pos10 += 1;
                   endif;

                   if umonth = 11;
                      sa_dolr11 += soactunet;
                      sa_wght11 += soactuwgt;
                      sa_volm11 += soactuvol;
                      sa_puri11 += soactupqt;
                      sa_othr11 += soactuoth;
                     sa_#pos11 += 1;
                   endif;

                   if umonth = 12;
                      sa_dolr12 += soactunet;
                      sa_wght12 += soactuwgt;
                      sa_volm12 += soactuvol;
                      sa_puri12 += soactupqt;
                      sa_othr12 += soactuoth;
                      sa_#pos12 += 1;
                   endif;
                   exsr updsuplpur;
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
                    sa_lastupd = lda_sysdat;
                    if umonth = 01;
                       sa_dolr01 = soactunet;
                       sa_wght01 = soactuwgt;
                       sa_volm01 = soactuvol;
                       sa_puri01 = soactupqt;
                       sa_othr01 = soactuoth;
                       sa_#pos01 = 1;
                    endif;

                    if umonth = 02;
                       sa_dolr02 =  soactunet;
                       sa_wght02 =  soactuwgt;
                       sa_volm02 =  soactuvol;
                       sa_puri02 =  soactupqt;
                       sa_othr02 =  soactuoth;
                       sa_#pos02 =  1;
                    endif;

                    if umonth = 03;
                       sa_dolr03 =  soactunet;
                       sa_wght03 =  soactuwgt;
                       sa_volm03 =  soactuvol;
                       sa_puri03 =  soactupqt;
                       sa_othr03 =  soactuoth;
                       sa_#pos03 =  1;
                    endif;

                    if umonth = 04;
                       sa_dolr04 =  soactunet;
                       sa_wght04 =  soactuwgt;
                       sa_volm04 =  soactuvol;
                       sa_puri04 =  soactupqt;
                       sa_othr04 =  soactuoth;
                       sa_#pos04 =  1;
                    endif;

                    if umonth = 05;
                       sa_dolr05 =  soactunet;
                       sa_wght05 =  soactuwgt;
                       sa_volm05 =  soactuvol;
                       sa_puri05 =  soactupqt;
                       sa_othr05 =  soactuoth;
                       sa_#pos05 =  1;
                    endif;

                    if umonth = 06;
                       sa_dolr06 =  soactunet;
                       sa_wght06 =  soactuwgt;
                       sa_volm06 =  soactuvol;
                       sa_puri06 =  soactupqt;
                       sa_othr06 =  soactuoth;
                       sa_#pos06 =  1;
                    endif;

                    if umonth = 07;
                       sa_dolr07 =  soactunet;
                       sa_wght07 =  soactuwgt;
                       sa_volm07 =  soactuvol;
                       sa_puri07 =  soactupqt;
                       sa_othr07 =  soactuoth;
                       sa_#pos07 =  1;
                    endif;

                    if umonth = 08;
                       sa_dolr08 =  soactunet;
                       sa_wght08 =  soactuwgt;
                       sa_volm08 =  soactuvol;
                       sa_puri08 =  soactupqt;
                       sa_othr08 =  soactuoth;
                       sa_#pos08 =  1;
                    endif;

                    if umonth = 09;
                       sa_dolr09 =  soactunet;
                       sa_wght09 =  soactuwgt;
                       sa_volm09 =  soactuvol;
                       sa_puri09 =  soactupqt;
                       sa_othr09 =  soactuoth;
                       sa_#pos09 =  1;
                    endif;

                    if umonth = 10;
                       sa_dolr10 =  soactunet;
                       sa_wght10 =  soactuwgt;
                       sa_volm10 =  soactuvol;
                       sa_puri10 =  soactupqt;
                       sa_othr10 =  soactuoth;
                       sa_#pos10 =  1;
                    endif;

                    if umonth = 11;
                       sa_dolr11 =  soactunet;
                       sa_wght11 =  soactuwgt;
                       sa_volm11 =  soactuvol;
                       sa_puri11 =  soactupqt;
                       sa_othr11 =  soactuoth;
                       sa_#pos11 =  1;
                    endif;

                    if umonth = 12;
                       sa_dolr12 =  soactunet;
                       sa_wght12 =  soactuwgt;
                       sa_volm12 =  soactuvol;
                       sa_puri12 =  soactupqt;
                       sa_othr12 =  soactuoth;
                       sa_#pos12 =  1;
                    endif;
                    exsr insert_suplpur;
                 endif;
                 exsr clssacursor;
             endif;
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
          callp K3S_Convert_ISO_Date(nt_expire:
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
          callp  K3S_Convert_ISO_Date(nt_expire:
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
          callp K3S_Convert_ISO_Date(nt_expire:
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
          callp K3S_Convert_ISO_Date(nt_expire:
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
                 @n_sequenx += 10;
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
             @n_sequenx += 10;
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
       callp K3S_Retrieve_Date_Time(lda_usrdat:
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
       exec sql
         select ta_codeds1
           into :tacodeds1
           from k_tablcod
           where ta_comp = :lda_comp and
                 ta_codetyp = 'TLE' and
                 ta_codeval = trimr(zz_program) ||
                              'FM' || @recd_frmt
           fetch first row only;

       //title record does exist
       if SQLState = SQLStateOk;

       //determine title length, and starting position for centering
          @length = %checkr(' ':tacodeds1);
          @start     = (50 - @length) / 2;

       //   center title
          zz_title = %subst(@blanks:1:(@start)) +
                     tacodeds1;

       //   title record does not exist, so provide message
       else;
          zz_title = @no_title;
       endif;

       endsr;

       begsr dclsvcursor;

       exec sql
         Declare svcursor Cursor
            for
            Select *
              from k_suplapv
              where sv_comp = :lda_comp and
                    sv_supl = :spsupl and
                    sv_suplsub = :spsuplsub
              for update of sv_lastupd,
                            sv_coment1,
                            sv_coment2,
                            sv_coment3,
                            sv_coment4,
                            sv_coment5,
                            sv_coment6,
                            sv_coment7;

       endsr;

       begsr opnsvcursor;
       exec sql
         open svcursor;
       endsr;

       begsr clssvcursor;
       exec sql
         close svcursor;
       endsr;

       begsr updsuplapv;

       exec sql
         update k_suplapv
           set sv_lastupd = :lda_cmpdat,
               sv_coment1 = :sv_coment1,
               sv_coment2 = :sv_coment2,
               sv_coment3 = :sv_coment3,
               sv_coment4 = :sv_coment4,
               sv_coment5 = :sv_coment5,
               sv_coment6 = :sv_coment6,
               sv_coment7 = :sv_coment7
           where current of svcursor;
       endsr;

       begsr insert_suplapv;

         exec sql
           insert into k_suplapv
             values (:suplapv_rec);

       endsr;

       begsr dclspcursor;

       exec sql
         Declare spcursor Cursor
            for
            Select *
              from k_suplier
              where sp_comp = :lda_comp and
                    sp_locn = :locn and
                    sp_supl = :supl and
                    sp_suplsub = :suplsub
              for update of sp_fxclst,
                            sp_fxcnxt,
                            sp_ordate,
                            sp_usern1;

       endsr;

       begsr opnspcursor;
       exec sql
         open spcursor;
       endsr;

       begsr clsspcursor;
       exec sql
         close spcursor;
       endsr;

       begsr updsuplier;

       exec sql
         update k_suplier
           set sp_fxclst = :sp_fxclst,
               sp_fxcnxt = :sp_fxcnxt,
               sp_ordate = :sp_ordate,
               sp_usern1 = :sp_usern1
           where current of spcursor;
       endsr;

       begsr dclbtcursor;

       exec sql
         Declare btcursor Cursor
            for
            Select *
              from k_buyrsot
              where bt_comp = :lda_comp and
                    bt_buyr = :buyr and
                    bt_locn = :locn and
                    bt_sysdate = :so_birth
              for update of bt_duefc#r,
                            bt_duefcvr,
                            bt_aprov#,
                            bt_aprovv,
                            bt_duerm#r,
                            bt_duermvr,
                            bt_duefb#r,
                            bt_duefbvr,
                            bt_duend#r,
                            bt_duendvr,
                            bt_notdu#r,
                            bt_notduvr;

       endsr;

       begsr opnbtcursor;
       exec sql
         open btcursor;
       endsr;

       begsr clsbtcursor;
       exec sql
         close btcursor;
       endsr;

       begsr updbuyrsot;

       exec sql
         update k_buyrsot
           set bt_duefc#r = :bt_duefc#r,
               bt_duefcvr = :bt_duefcvr,
               bt_aprov#  = :bt_aprov#,
               bt_aprovv  = :bt_aprovv,
               bt_duerm#r = :bt_duerm#r,
               bt_duermvr = :bt_duermvr,
               bt_duefb#r = :bt_duefb#r,
               bt_duefbvr = :bt_duefbvr,
               bt_duend#r = :bt_duend#r,
               bt_duendvr = :bt_duendvr,
               bt_notdu#r = :bt_notdu#r,
               bt_notduvr = :bt_notduvr
           where current of btcursor;
       endsr;

       begsr dclsocursor;

       exec sql
         Declare socursor Cursor
            for
            Select *
              from k_suplsoq
              where so_comp = :lda_comp and
                    so_buyr = :buyr and
                    so_locn = :locn and
                    so_supl = :supl and
                    so_suplsub = :suplsub and
                    so_soqseq# = :soqseq#
              for update of so_po#,
                            so_ordate,
                            so_expdelv,
                            so_soqtype;

       endsr;

       begsr opnsocursor;
       exec sql
         open socursor;
       endsr;

       begsr clssocursor;
       exec sql
         close socursor;
       endsr;

       begsr updsuplsoq;

       exec sql
         update k_suplsoq
           set so_po#     = :so_po#,
               so_ordate  = :so_ordate,
               so_expdelv = :so_expdelv,
               so_soqtype = :so_soqtype
           where current of socursor;
       endsr;

       begsr dclsacursor;

       exec sql
         Declare sacursor Cursor
            for
            Select *
              from k_suplpur
              where sa_comp = :lda_comp and
                    sa_locn = :zz_locn and
                    sa_supl = :zz_supl and
                    sa_suplsub = :zz_suplsub and
                    sa_year = :xx_year
              for update of sa_lastupd,
                            sa_dolr01,
                            sa_wght01,
                            sa_volm01,
                            sa_puri01,
                            sa_othr01,
                            sa_#pos01,
                            sa_dolr02,
                            sa_wght02,
                            sa_volm02,
                            sa_puri02,
                            sa_othr02,
                            sa_#pos02,
                            sa_dolr03,
                            sa_wght03,
                            sa_volm03,
                            sa_puri03,
                            sa_othr03,
                            sa_#pos03,
                            sa_dolr04,
                            sa_wght04,
                            sa_volm04,
                            sa_puri04,
                            sa_othr04,
                            sa_#pos04,
                            sa_dolr06,
                            sa_wght06,
                            sa_volm06,
                            sa_puri06,
                            sa_othr06,
                            sa_#pos06,
                            sa_dolr07,
                            sa_wght07,
                            sa_volm07,
                            sa_puri07,
                            sa_othr07,
                            sa_#pos07,
                            sa_dolr08,
                            sa_wght08,
                            sa_volm08,
                            sa_puri08,
                            sa_othr08,
                            sa_#pos08,
                            sa_dolr09,
                            sa_wght09,
                            sa_volm09,
                            sa_puri09,
                            sa_othr09,
                            sa_#pos09,
                            sa_dolr10,
                            sa_wght10,
                            sa_volm10,
                            sa_puri10,
                            sa_othr10,
                            sa_#pos10,
                            sa_dolr11,
                            sa_wght11,
                            sa_volm11,
                            sa_puri11,
                            sa_othr11,
                            sa_#pos11,
                            sa_dolr12,
                            sa_wght12,
                            sa_volm12,
                            sa_puri12,
                            sa_othr12,
                            sa_#pos12;
       endsr;

       begsr opnsacursor;
       exec sql
         open sacursor;
       endsr;

       begsr clssacursor;
       exec sql
         close sacursor;
       endsr;

       begsr updsuplpur;

       exec sql
         update k_suplpur
           set sa_lastupd = :sa_lastupd,
               sa_dolr01  = :sa_dolr01,
               sa_wght01  = :sa_wght01,
               sa_volm01  = :sa_volm01,
               sa_puri01  = :sa_puri01,
               sa_othr01  = :sa_othr01,
               sa_#pos01  = :sa_#pos01,

               sa_dolr02  = :sa_dolr02,
               sa_wght02  = :sa_wght02,
               sa_volm02  = :sa_volm02,
               sa_puri02  = :sa_puri02,
               sa_othr02  = :sa_othr02,
               sa_#pos02  = :sa_#pos02,

               sa_dolr03  = :sa_dolr03,
               sa_wght03  = :sa_wght03,
               sa_volm03  = :sa_volm03,
               sa_puri03  = :sa_puri03,
               sa_othr03  = :sa_othr03,
               sa_#pos03  = :sa_#pos03,

               sa_dolr04  = :sa_dolr04,
               sa_wght04  = :sa_wght04,
               sa_volm04  = :sa_volm04,
               sa_puri04  = :sa_puri04,
               sa_othr04  = :sa_othr04,
               sa_#pos04  = :sa_#pos04,

               sa_dolr05  = :sa_dolr05,
               sa_wght05  = :sa_wght05,
               sa_volm05  = :sa_volm05,
               sa_puri05  = :sa_puri05,
               sa_othr05  = :sa_othr05,
               sa_#pos05  = :sa_#pos05,

               sa_dolr06  = :sa_dolr06,
               sa_wght06  = :sa_wght06,
               sa_volm06  = :sa_volm06,
               sa_puri06  = :sa_puri06,
               sa_othr06  = :sa_othr06,
               sa_#pos06  = :sa_#pos06,

               sa_dolr07  = :sa_dolr07,
               sa_wght07  = :sa_wght07,
               sa_volm07  = :sa_volm07,
               sa_puri07  = :sa_puri07,
               sa_othr07  = :sa_othr07,
               sa_#pos07  = :sa_#pos07,

               sa_dolr08  = :sa_dolr08,
               sa_wght08  = :sa_wght08,
               sa_volm08  = :sa_volm08,
               sa_puri08  = :sa_puri08,
               sa_othr08  = :sa_othr08,
               sa_#pos08  = :sa_#pos08,

               sa_dolr09  = :sa_dolr09,
               sa_wght09  = :sa_wght09,
               sa_volm09  = :sa_volm09,
               sa_puri09  = :sa_puri09,
               sa_othr09  = :sa_othr09,
               sa_#pos09  = :sa_#pos09,

               sa_dolr10  = :sa_dolr10,
               sa_wght10  = :sa_wght10,
               sa_volm10  = :sa_volm10,
               sa_puri10  = :sa_puri10,
               sa_othr10  = :sa_othr10,
               sa_#pos10  = :sa_#pos10,

               sa_dolr11  = :sa_dolr11,
               sa_wght11  = :sa_wght11,
               sa_volm11  = :sa_volm11,
               sa_puri11  = :sa_puri11,
               sa_othr11  = :sa_othr11,
               sa_#pos11  = :sa_#pos11,

               sa_dolr12  = :sa_dolr12,
               sa_wght12  = :sa_wght12,
               sa_volm12  = :sa_volm12,
               sa_puri12  = :sa_puri12,
               sa_othr12  = :sa_othr12,
               sa_#pos12  = :sa_#pos12

               where current of sacursor;
       endsr;

       begsr insert_suplpur;

        exec sql
          insert into k_suplpur
            values (:suplpur_rec);

       endsr;

      /end-free

