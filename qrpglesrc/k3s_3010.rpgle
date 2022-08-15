      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
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
      *d/copy k3s_c050

      * ---------------------------------- Display headings for history
     d/copy k3s_c140

      * --------------------------------- common D specs
      *d/copy k3s_c270

      * --------------------------------- L and R adjust for prod ID
     d/copy k3s_c280

      * -------------------------------------------------- parameters passed
     d accdem          s              7  0 dim(52)
     d*updtype         s                   like(pr_comp)
     d*addrep          s                   like(pr_comp)
     d*usrstat         s                   like(pr_usrstat)
     d*frezend         s                   like(zz_frezend)
     d*setfore         s                   like(zz_setfore)
     d*multply         s                   like(zz_multply)
     d*tlocn           s                   like(pr_locn)
     d*tsupl           s                   like(pr_supl)
     d*tsuplsub        s                   like(pr_suplsub)
     d xx_tosupl       s                   like(pr_supl)
     d xx_tosub        s                   like(pr_suplsub)
     d xx_frsupl       s                   like(pr_supl)
     d xx_frsub        s                   like(pr_suplsub)
     d xx_forcyr       s                   like(pr_forcyr)
     d xx_forcper      s                   like(pr_forcper)
     d zz_forcyr       s                   like(pr_forcyr)
     d zz_forcper      s                   like(pr_forcper)
     d*supla           s                   like(pr_supl)
     d*suplsuba        s                   like(pr_suplsub)
     d*tprod           s                   like(pr_prod)
     d xx_comp         s                   like(pr_comp)
     d savebirth       s                   like(pr_birth)
     d xx_year         s                   like(ph_year)
     d blank           s                   like(pr_comp)
     d*histype         s                   like(ph_histype)
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

     d combine         s              1
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
     d*program2        s                   like(program)
     d program3        s                   like(program)
     d pgm             s                   like(program)

     d pf_comp         s              1
     d pf_locn         s              5
     d pf_supl         s             10
     d pf_suplsub      s             10
     d pf_prod         s             25
     d pf_birth        s               D
     d pf_birthtm      s               T
     d pf_chgtype      s              1
     d pf_statbef      s              1
     d pf_stataft      s              1
     d pf_sysstat      s              1
     d pf_seasbef      s             10
     d pf_seasaft      s             10
     d pf_user         s             10
     d pf_workstn      s             10
     d pf_program      s             10
     d pf_chgdesc      s             20
     d pf_avgbef       s              9  2
     d pf_avgaft       s              9  2
     d pf_avgdiff      s              9  2
     d pf_devpbef      s              3  1
     d pf_devpaft      s              3  1

     d time_stamp      s               z   inz
     d time            s               t   timfmt(*iso)
     d date            s               d   datfmt(*iso)
     d errors          s              1
     d send_error      s              1
     d iso_error       s              1p 0                                      Error date entered
     d iso_end         s               d   datfmt(*iso)                         batch end date
     d flag3_test      s              1  0
     d re_display      s              1
     d finished        s              1

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
     d @8_sysdate      s             10                                         product
     d @8_suplusr      s             10                                         product
     d @8_suplusb      s             10                                         product
     d @8_soqseq#      s              5                                         product
     d @8_po#          s             10                                         product
     d @8_status       s              1                                         product
     d @8_batch        s              7                                         product


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

     d producta_key    DS                  likerec(rk_product:*key)
     d productg_key    DS                  likerec(r1_product:*key)
     d prodsoqb_key    DS                  likerec(rk_prodsoq:*key)
     d prodh52b_key    DS                  likerec(rk_prodh52:*key)
     d prodhisb_key    DS                  likerec(rk_prodhis:*key)
     d suplsoqa_key    DS                  likerec(rk_suplsoq:*key)

      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3010        PI
     d  comp                          1
     d  buyr                          5
     d  locn                          5
     d  supl                         10
     d  suplsub                      10
     d  prod                         25
     d  @updated                      1  0
     d  @returned                     3  0
     d  mode                          8
     d  soqseq#                       5  0
     d  prodseq                      40
     d  program                      10
     d  program2                     10
     d  checktype                     2
     d  supla                        10
     d  suplsuba                     10
     d  rollupdown                    1
     d  mode1                         1
     d  histype                       1  0
     d  updtype                       1
     d  tlocn                         5
     d  tsupl                        10
     d  tsuplsub                     10
     d  tprod                        25
     d  addrep                        1
     d  usrstat                       1
     d  frezend                      10
     d  setfore                       1
     d  multply                       9  4


      * ------------------------------------------------------- Once Routine
      * once routine
      /free
       exsr $_once;
       key_press = enter_key;
       xx_comp = *hival;
       diff19 = *off;

       if updtype = 'O';
          pf_chgdesc = *blanks;
          pf_chgdesc = 'User made change    ';
       endif;
       if updtype = 'B';
          pf_chgdesc = *blanks;
          pf_chgdesc = 'K3S_3011 made change';
       endif;


       // get user update authority flag
       // call module to obtain user update authority flag
       if updtype = 'O';
          @usr_updat = 0;
          callp K3S_9050(zz_program:
                         @usr_updat);
          *in94 = (@usr_updat = 0);
          *in97 = *off;
          if (@usr_updat = 0) or
             (mode1 = 'V');
             *in97 = *on;
          endif;
       endif;

       // prime F8=User exit text
       exsr $_usr_exit;

       // get screen title
       @recd_frmt = '1R';
       exsr $_get_titl;

       // get time formatted
       if updtype = 'O';
          exsr $_get_time;
       endif;


       // -------------------------------- display lead time transactions loop

       // continue until user decides to get out
       dow (key_press <> f03_key) and
           (finished = *off);

       // controlled loop for comand key display
          dou (key_press <> f24_key) and
              (re_display = *off);

       // if errors exist, and user set for alarm, sound alarm
              if (errors = *on) and
                 (lda_alarm = 1);
                 *in98 = *on;
                 exsr $_pass_msg;
              else;
                 *in98 = *off;
                 exsr $_pass_msg;
              endif;

              if errors = *off;
                 *in61 = *on;
              endif;
       // ------------------------------------- lead time transactions screens
              if (*in(tablpsuNOT) = *on) and
                 (lda_window = 1) or
                 (*in(locatnfNOT) = *on) and
                 (lda_window = 1) or
                 (*in(locatntNOT) = *on) and
                 (lda_window = 1) or
                 (*in(tablphtNOT) = *on) and
                 (lda_window = 1);
                 if updtype = 'O';
                    exsr $_cur_sel;
                 endif;
                 if updtype = 'B';
                    key_press = f03_key;
                 endif;
                 *in(tablpsuNOT) = *off;
                 *in(tablphtNOT) = *off;
                 *in(locatnfNOT) = *off;
                 *in(locatntNOT) = *off;
              else;
                 if updtype = 'O';
                    write k3_ctl_msg;
                    exfmt k3_3010_1r;
       // Right adjust
                    if rightadj = *on;
                       prodin = zz_frprod;
                       callp K3S_M170(prodin:
                                      prodlen:
                                      prodout);

                        zz_frprod  = prodout;

                        prodin = zz_toprod;
                        callp K3S_M170(prodin:
                                       prodlen:
                                       prodout);
                        zz_toprod  = prodout;
                    endif;
       // Right adjust
                 endif;
              endif;
       // user has pressed F15=add history
              if key_press = f15_key or
                 addrep = 'A';
                 add = *on;
                 replace = *off;
              endif;

       // user has pressed F19=replace history
              if key_press = f19_key or
                 addrep = 'R';
                 add = *off;
                 replace = *on;
              endif;
       // user has pressed F03=Exit to exit without update
              if key_press = f03_key;
                 finished = *on;
                 @updated  = 0;
                 @returned = 3;
              endif;

       // user has pressed F12=Previous to return without update
              if key_press = f12_key;
                 finished = *on;
                 @updated  = 0;
                 @returned = 12;
              endif;


       // Function key 4 (prompt) was pressed.
              if key_press = f04_key;
       // Check posistion of cursor on the screen.
                 exsr $_cur_pos;
       // Call appropriate selection program if applicable.
                 exsr $_cur_sel;
              endif;
       // subfile message control
              *in99 = *off;

       // sound alarm indicator
              *in98 = *off;

       // set off error
              errors = *off;


       // re-display after valid command key list
              re_display = *off;

       // display valid comand keys
              if key_press = f24_key;
       // get time formatted
                 exsr $_get_time;
                 exfmt K3_3010_1f;
       // user has pressed F03=Exit to exit without update
                 if key_press = f03_key;
                    finished = *on;
                    @updated  = 0;
                    @returned = 3;
                 endif;

       // user has pressed F12=Previous to return without update
                 if key_press = f12_key;
                    finished = *on;
                    @updated  = 0;
                    @returned = 12;
                 endif;


       //******************************************
       // maintain user preferences
       //
                 if key_press = f24_key;
                    @returned = 0;
                    callp K3S_9040CL(zz_program:
                                     @returned);
                    if @returned = 3;
                       finished = *on;
                       key_press = f03_key;
                    endif;

       // retrieve data area *lda
                    exsr $_get_lda;
                 endif;

       //******************************************


       // re-display if user presses enter key, else user selects function key
                 if key_press = enter_key;
                    re_display = *on;
                 endif;
              endif;

          enddo;


          if (zz_frlocn  <> sz_frlocn)   or
             (zz_frsupl  <> sz_frsupl)   or
             (zz_frsub   <> sz_frsub)    or
             (zz_frprod  <> sz_frprod)   or
             (zz_tolocn  <> sz_tolocn)   or
             (zz_tosupl  <> sz_tosupl)   or
             (zz_tosub   <> sz_tosub)    or
             (zz_toprod  <> sz_toprod)   or
             (zz_histype <> sz_histype)  or
             (zz_usrstat <> sz_usrstat)  or
             (zz_frezend <> sz_frezend)  or
             (zz_setfore <> sz_setfore)  or
             (key_press = f15_key) or
             (key_press = f19_key) or
             (addrep = 'A') or
             (addrep = 'R');

       //    edit location entry
             exsr $_edt_locn;
       //    edit supplier entry
             if errors = *off;
                exsr $_edt_supl;
             endif;
       //    edit product entry
             if errors = *off;
                exsr $_edt_prod;
             endif;
       //    edit history type entry
             if errors = *off;
                exsr $_edt_type;
             endif;
       //    edit user status
             if errors = *off;
                exsr $_edt_sta1;
             endif;
       //    edit date
             clear iso_end;
             if errors = *off and zz_frezend > *blanks or
                errors = *off and zz_usrstat = 'F';
                if updtype = 'O';
                   exsr $_edt_endz;
                endif;
                if updtype = 'B' and
                  frezend <> *blanks;
                  iso_end = %date(frezend:*usa);
                endif;
             endif;
            sz_frlocn = zz_frlocn;
            sz_frsupl = zz_frsupl;
            sz_frsub  = zz_frsub;
            sz_frprod = zz_frprod;
            sz_tolocn = zz_tolocn;
            sz_tosupl = zz_tosupl;
            sz_tosub  = zz_tosub;
            sz_toprod = zz_toprod;
            sz_histype = zz_histype;
            sz_usrstat = zz_usrstat;
            sz_frezend = zz_frezend;
            sz_setfore = zz_setfore;
          endif;

       // f01 HELP exit
      /end-free
     d/copy K3S_C192
      /free

       // f08 user exit
          if key_press = f08_key;
             @8_locn = locn;
             @8_buyr = buyr;
             @8_supl = supl;
             @8_suplsub = suplsub;
             @8_prod = prod;
             exsr $_f8_exit;
          endif;

       // end of main loop
          if updtype = 'O' and
             diff19 = *on;
             dou key_press = f12_key or
                 key_press = f19_key;
                 errors = *on;
                 write k3_ctl_msg;
                 write k3_3010_1r;
                 exfmt k3_3010_wn;
                 if key_press = f12_key;
                    finished = *on;
                    @updated  = 0;
                    @returned = 12;
                    diff19    = *off;
                 endif;
                 if key_press = f19_key;
                    errors = *off;
                    diff19 = *on;
                 endif;
             enddo;
          endif;
       // user has pressed F12=Previous to return without update
          if (key_press = f03_key) or
             (key_press = f12_key);
             finished = *on;
             @updated = 1;
          else;
             if (add = *on) or
                (replace = *on);
                if errors = *off;
                   exsr $_hist_chk;
                   if updtype = 'O' and
                      combine = *on;
                      exsr $_combine;
                   endif;
                endif;
             endif;
          endif;
      /end-free
     d/copy k3s_c250
      /free
          if updtype = 'B';
             key_press = f03_key;
             finished = *on;
             @updated = 1;
          endif;
       enddo;

       // --------------------------------------------------- End of Main Loop

       // finished, set on LR
       if updtype = 'O';
          close k3s_3010fm;
       //                   close     k3s_3010fm                           62
       endif;
       *inlr = *on;

       // ***************************************************** End of program

       // /////////////////////////////////////////////////////// Once routine

       begsr $_once;
       if updtype = 'O';
          open k3s_3010fm;
       //                   open      k3s_3010fm                           62
       endif;
       addmsg = *off;
       if (checktype = *blanks);
          zz_modedsp = %subst('  -' : 1) + 'all- ';
       else;
          zz_modedsp = %subst('  -' : 1) +
                       checktype + '-  ';
       endif;
       if mode = '-select-';
          zz_modedsp = %subst(mode : 1);
       endif;

       // prime program message queue
       msgpgq = '*';

       zz_modedsp = mode;
       // retrieve data area *lda, and format information
       if updtype = 'O';
          exsr $_get_lda;
       endif;

       chain(n) (comp:locn) k_locatnsa;
       if updtype = 'B' and
          locn = *blanks;
          *in15 = *off;
       endif;
       // prime program id
       zz_program = psds_progm;

       // prime program message queue
       msgpgq = '*';

       // prime program id
       zz_program = psds_progm;

       // prime company code
       zz_compcod = lda_compcd;

       // prime user id
       zz_user = psds_user;

       // prime table code id

       // set finished indicator to off
       finished = *off;

       // set error off
       errors = *off;

       // position cursor to location
       *in54 = *on;
       *in61 = *on;
       *in25 = *off;

       // get product
       producta_key.pr_comp = comp;
       producta_key.pr_locn = locn;
       producta_key.pr_supl = supl;
       producta_key.pr_suplsub = suplsub;
       producta_key.pr_prod = prod;
       chain(n) %kds(producta_key) k_producta;
       //
       prodsoqb_key.pq_comp = comp;
       prodsoqb_key.pq_buyr = buyr;
       prodsoqb_key.pq_locn = locn;
       prodsoqb_key.pq_supl = supl;
       prodsoqb_key.pq_suplsub = suplsub;
       prodsoqb_key.pq_soqseq# = soqseq#;
       prodsoqb_key.pq_prod = prod;
       chain(n) %kds(prodsoqb_key) k_prodsoqb;
       if updtype = 'O';
          zz_frlocn  = locn;
          zz_frbuyr  = pr_buyr;
          zz_tobuyr  = pr_buyr;
          zz_frforci = pr_forcint;
          zz_toso#1  = 001;
          zz_toso#   = pq_soqseq#;
          zz_frso#   = pq_soqseq#;
          zz_toforci = pr_forcint;
          zz_frsupl  = supl;
          zz_frsub   = suplsub;
          zz_frprod  = prod;
          zz_frdesc1 = pr_desc1;
          zz_frdesc2 = pr_desc2;
          zz_tolocn  = locn;
          zz_tosupl  = supl;
          zz_tosub   = suplsub;
          zz_toprod  = prod;
          zz_todesc1 = pr_desc1;
          zz_todesc2 = pr_desc2;
          zl_frlocn  = locn;
          zl_frsupl  = supl;
          zl_frsub   = suplsub;
          zl_frprod  = prod;
          zl_frdesc1 = pr_desc1;
          zl_tolocn  = locn;
          zl_tosupl  = supl;
          zl_tosub   = suplsub;
          zl_toprod  = prod;
          zl_todesc1 = pr_desc1;
          zz_multply = 1.0000;
          zl_multply = 1.0000;
          zz_histype = histype;
          zl_histype = histype;
          sz_histype = histype;
          zz_setfore = '0';
          sz_setfore = '0';
          *in80 = *off;
          exsr $_hist_ck1;
          @returned = *zeros;
          key_press = enter_key;
       endif;
       if updtype = 'B';
          zz_frlocn  = locn;
          zz_frbuyr  = pr_buyr;
          zz_tobuyr  = pr_buyr;
          zz_frforci = pr_forcint;
          zz_toso#1  = 001;
          zz_toso#   = pq_soqseq#;
          zz_frso#   = pq_soqseq#;
          zz_toforci = pr_forcint;
          zz_frsupl  = supl;
          zz_frsub   = suplsub;
          zz_frprod  = prod;
          zz_tolocn  = tlocn;
          zz_tosupl  = tsupl;
          zz_tosub   = tsuplsub;
          zz_toprod  = tprod;
          zl_frlocn  = locn;
          zl_frsupl  = supl;
          zl_frsub   = suplsub;
          zl_frprod  = prod;
          zl_tolocn  = tlocn;
          zl_tosupl  = tsupl;
          zl_tosub   = tsuplsub;
          zl_toprod  = tprod;
          if multply = 0;
             zz_multply = 1.0000;
             zl_multply = 1.0000;
          else;
             zz_multply = multply;
             zl_multply = multply;
          endif;
          if setfore = '1';
             zz_setfore = setfore;
          else;
             zz_setfore = '0';
          endif;
          if frezend <> *blanks and
             frezend <> '0001-01-01';
             zz_frezend = frezend;
          else;
             zz_frezend = *blanks;
          endif;
          if usrstat <> *blanks;
             zz_usrstat = usrstat;
          else;
             zz_usrstat = *blanks;
          endif;
          zz_histype = histype;
          zl_histype = histype;
          sz_histype = histype;
          *in80 = *off;
          exsr $_hist_ck1;
          @returned = *zeros;
          key_press = enter_key;
       endif;
       //**** Right adjust
       // Get Product Right adjust info.
       ta_comp = lda_comp;
       ta_codetyp = 'APP';
       ta_codeval = 'PRODUCT   RIGHT_ADJ ';
       rightadj = *off;
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;
       if %found(k_tablcoda);
          if ta_flag1 = 1;
             rightadj = *on;
             prodlen = ta_number1;
          else;
             rightadj = *off;
             prodlen = 0;
          endif;
       else;
          rightadj = *off;
          prodlen = 0;
       endif;

       //**** Right adjust

       // determine if updating Combined Supplier Product record needed
       ta_codetyp = 'APP';
       ta_codeval = 'K3S_3010  COMBINE   ';
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;
       if %found(k_tablcoda) and ta_flag1 = 1;
          combine  = *on;
       else;
          combine  = *off;
       endif;

       endsr;

       //////////////////////////////////////////////////////// Edit supplier

       begsr $_edt_supl;

       // set read indicator off
       errors = *off;

       // get from supplier record
       setll (comp:zz_frsupl:zz_frsub) k_suplierc;
       reade(n) (comp:zz_frsupl:zz_frsub) k_suplierc;

       // supplier record not found, send back error
       if %eof(k_suplierc);
          errors = *on;
          send_error = *on;
          *in56 = *on;
          *in61 = *off;
          @msg_id = 'K3_2000';
          @msg_text = zz_frsupl;
          if addmsg = *off;
             exsr $_add_msg;
          endif;
       endif;

       if errors = *off;
       // get to supplier record
          setll (comp:zz_tosupl:zz_tosub) k_suplierc;
          reade(n) (comp:zz_tosupl:zz_tosub) k_suplierc;

       // supplier record not found, send back error
          if %eof(k_suplierc);
             errors = *on;
             send_error = *on;
             *in57 = *on;
             *in61 = *off;
             @msg_id   = 'K3_2000';
             @msg_text = zz_tosupl;
             if addmsg = *off;
                exsr $_add_msg;
             endif;
          endif;
       endif;


       endsr;

       //////////////////////////////////////////////////////// Edit location

       begsr $_edt_locn;

       // set read indicator off
       errors = *off;

       if zz_frlocn <> *blanks;
          chain(n) (comp:zz_frlocn) k_locatnsa;

       // supplier record not found, send back error
          if %eof(k_locatnsa);
             errors = *on;
             send_error = *on;
             *in58 = *on;
             *in61 = *off;
             @msg_id   = 'K3_8020';
             @msg_text = zz_frlocn;
             if addmsg = *off;
                exsr $_add_msg;
             endif;
          endif;
       endif;

       if (errors = *off) and
          (zz_tolocn <> *blanks);
       // get to supplier record
          chain(n) (comp:zz_tolocn) k_locatnsa;

       // supplier record not found, send back error
          if %eof(k_locatnsa);
             errors = *on;
             send_error = *on;
             *in59 = *on;
             *in61 = *off;
             @msg_id = 'K3_8020';
             @msg_text = zz_tolocn;
             if addmsg = *off;
                exsr $_add_msg;
             endif;
          endif;
       endif;

       if (errors = *off) and
          (zz_frlocn = *blanks) and
          (zz_tolocn <> *blanks);

          errors = *on;
          *in58 = *on;
          *in61 = *off;
          @msg_id   = 'K3_9999';
          @msg_text = 'from loc blank to loc <> blank';
          if addmsg = *off;
             exsr $_add_msg;
          endif;
       endif;

       if (errors = *off) and
          (zz_tolocn = *blanks) and
          (zz_frlocn <> *blanks);

          errors = *on;
          *in58 = *on;
          *in61     = *off;
          @msg_id   = 'K3_9999';
          @msg_text = 'to loc blank from loc <> blank';
          if addmsg = *off;
             exsr $_add_msg;
          endif;
       endif;


       endsr;

       //////////////////////////////////////////////////////// Edit product

       begsr $_edt_prod;

       // set read indicator off
       errors = *off;

       if zz_frlocn = *blanks;
          setll (comp:zz_frprod) k_productg;
          dou %eof(k_productg) or
              pr_supl = zz_frsupl;
              reade(n) (comp:zz_frprod) k_productg;
          enddo;
       else;
          producta_key.pr_comp = comp;
          producta_key.pr_locn = zz_frlocn;
          producta_key.pr_supl = zz_frsupl;
          producta_key.pr_suplsub = zz_frsub;
          producta_key.pr_prod = zz_frprod;
          chain(n) %kds(producta_key) k_producta;
       endif;

       // supplier record not found, send back error
       if not %found(k_producta) or %eof(k_productg);
          errors = *on;
          send_error = *on;
          *in60 = *on;
          *in61 = *off;
          @msg_id = 'K3_3080';
          @msg_text = zz_frprod;
          if addmsg = *off;
             exsr $_add_msg;
          endif;
       else;
          zz_forcast = pr_forcast * zz_multply;
          zz_fordevp = pr_fordevp;
          zz_forserr = pr_forserr;
          zz_accsale = pr_accsale * zz_multply;
          zz_accouts = pr_accouts * zz_multply;
          //***********        eval      zz_accdem  = pr_accdem
          zz_accdem  = zz_accsale + zz_accouts;
          zz_fstslyr = pr_fstslyr;
          zz_fstslpr = pr_fstslpr;
          xx_forcyr  = pr_forcyr;
          xx_forcper = pr_forcper;
          zz_seasonl = pr_seasonl;
          zz_frdelt  = pr_deltcnt;
          zz_frbuyr  = pr_buyr;
          zz_frforci = pr_forcint;
          zz_frdesc1 = pr_desc1;
          zz_frdesc2 = pr_desc2;
          sz_frsupl  = pr_supl;
          sz_frsub   = pr_suplsub;
          sz_frprod  = pr_prod;
          sz_usrstat = *blanks;
          sz_frezend = *blanks;
          sz_setfore = '0';
       endif;

       if errors = *off;
       // get to supplier record
          if zz_tolocn = *blanks;
             setll (comp:zz_toprod) k_productg;
             dou %eof(k_productg) or
                 pr_supl = zz_tosupl;

                 reade(n) (comp:zz_toprod) k_productg;
             enddo;
          else;
             producta_key.pr_comp = comp;
             producta_key.pr_locn = zz_tolocn;
             producta_key.pr_supl = zz_tosupl;
             producta_key.pr_suplsub = zz_tosub;
             producta_key.pr_prod = zz_toprod;
             chain(n) %kds(producta_key) k_producta;
          endif;

       // supplier record not found, send back error
          if not %found(k_producta) or %eof(k_productg);
             errors = *on;
             send_error = *on;
             *in61 = *on;
             @msg_id   = 'K3_3080';
             @msg_text = zz_toprod;
             if addmsg = *off;
                exsr $_add_msg;
             endif;
          else;
             zz_forcyr  = pr_forcyr;
             zz_forcper = pr_forcper;
             zz_tobuyr  = pr_buyr;
             zz_toforci = pr_forcint;
             zz_todesc1 = pr_desc1;
             zz_todesc2 = pr_desc2;
             zz_todelt  = pr_deltcnt;
             sz_tosupl  = pr_supl;
             sz_tosub   = pr_suplsub;
             sz_toprod  = pr_prod;
          endif;
       endif;

       if errors = *off;
          if zz_frforci <> zz_toforci and
             add = *on;

             errors = *on;
             *in61 = *on;
             @msg_id   = 'K3_9999';
             @msg_text = 'Forecast types not the same';
             if addmsg = *off;
                exsr $_add_msg;
             endif;
           endif;
       endif;

       if errors = *off;
          if zz_frforci <> zz_toforci and
             replace = *on;

             diff19 = *on;
          endif;
       endif;




       endsr;

       ///////////////////////////////////////////////// get history

       begsr $_hist_ck1;

       ta_comp = comp;
       ta_codetyp = 'PHT';
       ta_codeval = %editc(zz_histype:'X');
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;
       zz_histdsc = ta_codeds1;
       endsr;
       //////////////////////////////////////////////// get history

       begsr $_hist_chk;

       if diff19 = *on;
          exsr $_fix_hist;
       endif;
       domany = *off;
       addmsg = *on;
       if (sz_frlocn = *blanks) and
          (sz_tolocn = *blanks);

          setll (xx_comp) k_locatnsa;
          read(n) k_locatnsa;
          setll (comp) k_locatnsa;
          reade(n) (comp) k_locatnsa;
          if not %eof(k_locatnsa);
             zz_frlocn = lc_locn;
             zz_tolocn = lc_locn;
          endif;
       endif;
       // edit location entry
       exsr $_edt_locn;
       // edit supplier entry
       if errors = *off;
          exsr $_edt_supl;
       endif;
       // edit product entry
       if errors = *off;
          exsr $_edt_prod;
          if (errors = *on) and
             (sz_frlocn = *blanks) and
             (sz_tolocn = *blanks);

             dou errors = *off or
                %eof(k_locatnsa);
                if (sz_frlocn = *blanks) and
                   (sz_tolocn = *blanks) and
                   (errors = *on);
                   reade(n) (comp) k_locatnsa;
                   if not %eof(k_locatnsa);
                      zz_frlocn = lc_locn;
                      zz_tolocn = lc_locn;
                      errors = *off;
                      exsr $_edt_prod;
                   endif;
                endif;
             enddo;
          endif;
       endif;
       // edit history type entry
       if errors = *off;
          exsr $_edt_type;
       endif;
       sz_frsupl = zz_frsupl;
       sz_frsub  = zz_frsub;
       sz_frprod = zz_frprod;
       sz_tosupl = zz_tosupl;
       sz_tosub  = zz_tosub;
       sz_toprod = zz_toprod;
       sz_histype = zz_histype;
       sz_usrstat = zz_usrstat;
       sz_frezend = zz_frezend;
       sz_setfore = zz_setfore;
       if errors = *off;
          if zz_multply = 0;
             zz_multply = 1.0000;
          endif;
          if zz_frforci < 52;
             setll (*hival) k_prodhisb;
             prodhisb_key.ph_comp = comp;
             prodhisb_key.ph_locn = zz_frlocn;
             prodhisb_key.ph_supl = zz_frsupl;
             prodhisb_key.ph_suplsub = zz_frsub;
             prodhisb_key.ph_prod = zz_frprod;
             prodhisb_key.ph_forcint = zz_frforci;
             prodhisb_key.ph_histype = zz_histype;
             setll %kds(prodhisb_key:7) k_prodhisb;
             reade(n) %kds(prodhisb_key:7) k_prodhisb;

             dow not %eof(k_prodhisb);

                 per_hist(01) = ph_per01  * zz_multply;
                 per_hist(02) = ph_per02  * zz_multply;
                 per_hist(03) = ph_per03  * zz_multply;
                 per_hist(04) = ph_per04  * zz_multply;
                 per_hist(05) = ph_per05  * zz_multply;
                 per_hist(06) = ph_per06  * zz_multply;
                 per_hist(07) = ph_per07  * zz_multply;
                 per_hist(08) = ph_per08  * zz_multply;
                 per_hist(09) = ph_per09  * zz_multply;
                 per_hist(10) = ph_per10  * zz_multply;
                 per_hist(11) = ph_per11  * zz_multply;
                 per_hist(12) = ph_per12  * zz_multply;
                 per_hist(13) = ph_per13  * zz_multply;
                 xx_year = ph_year;
                 exsr $_hist_chg;
                 xx_year -= 1;
                 prodhisb_key.ph_comp = comp;
                 prodhisb_key.ph_locn = zz_frlocn;
                 prodhisb_key.ph_supl = zz_frsupl;
                 prodhisb_key.ph_suplsub = zz_frsub;
                 prodhisb_key.ph_prod = zz_frprod;
                 prodhisb_key.ph_forcint = zz_frforci;
                 prodhisb_key.ph_histype = zz_histype;
                 prodhisb_key.ph_year = xx_year;
                 setll %kds(prodhisb_key) k_prodhisb;
                 reade(n) %kds(prodhisb_key) k_prodhisb;
                 if (sz_frlocn = *blanks) and
                    (sz_tolocn = *blanks) and
                    %eof(k_prodhisb);
                        dou not %eof(k_prodhisb) or
                            %eof(k_locatnsa);
                            reade(n) (comp) k_locatnsa;
                            if not %eof(k_locatnsa);
                               zz_frlocn = lc_locn;
                               zz_tolocn = lc_locn;
                               if pr_forcint < 52;
                                  setll (*hival) k_prodhisb;
                                  prodhisb_key.ph_comp = comp;
                                  prodhisb_key.ph_locn = zz_frlocn;
                                  prodhisb_key.ph_supl = zz_frsupl;
                                  prodhisb_key.ph_suplsub = zz_frsub;
                                  prodhisb_key.ph_prod = zz_frprod;
                                  prodhisb_key.ph_forcint = zz_frforci;
                                  prodhisb_key.ph_histype = zz_histype;
                                  setll %kds(prodhisb_key:7) k_prodhisb;
                                  reade(n) %kds(prodhisb_key:7) k_prodhisb;
                               endif;
                               domany = *off;
                            endif;
                        enddo;
                        if %eof(k_locatnsa);
                           domany = *on;
                           zz_tolocn = *blanks;
                           zz_frlocn = *blanks;
                           zl_tolocn = 'All';
                           zl_frlocn = 'All';
                           leave;
                        endif;
                 else;
                        domany = *on;
                 endif;
             enddo;
             add = *off;
             replace = *off;
          endif;

          if zz_frforci = 52;
             setll (*hival) k_prodh52b;
             prodh52b_key.pw_comp = comp;
             prodh52b_key.pw_locn = zz_frlocn;
             prodh52b_key.pw_supl = zz_frsupl;
             prodh52b_key.pw_suplsub = zz_frsub;
             prodh52b_key.pw_prod = zz_frprod;
             prodh52b_key.pw_forcint = zz_frforci;
             prodh52b_key.pw_histype = zz_histype;
             setll %kds(prodh52b_key:7) k_prodh52b;
             reade(n) %kds(prodh52b_key:7) k_prodh52b;
             dow not %eof(k_prodh52b);

                 per_histw(01) = pw_per01 * zz_multply;
                 per_histw(02) = pw_per02 * zz_multply;
                 per_histw(03) = pw_per03 * zz_multply;
                 per_histw(04) = pw_per04 * zz_multply;
                 per_histw(05) = pw_per05 * zz_multply;
                 per_histw(06) = pw_per06 * zz_multply;
                 per_histw(07) = pw_per07 * zz_multply;
                 per_histw(08) = pw_per08 * zz_multply;
                 per_histw(09) = pw_per09 * zz_multply;
                 per_histw(10) = pw_per10 * zz_multply;
                 per_histw(11) = pw_per11 * zz_multply;
                 per_histw(12) = pw_per12 * zz_multply;
                 per_histw(13) = pw_per13 * zz_multply;
                 per_histw(14) = pw_per14 * zz_multply;
                 per_histw(15) = pw_per15 * zz_multply;
                 per_histw(16) = pw_per16 * zz_multply;
                 per_histw(17) = pw_per17 * zz_multply;
                 per_histw(18) = pw_per18 * zz_multply;
                 per_histw(19) = pw_per19 * zz_multply;
                 per_histw(20) = pw_per20 * zz_multply;
                 per_histw(21) = pw_per21 * zz_multply;
                 per_histw(22) = pw_per22 * zz_multply;
                 per_histw(23) = pw_per23 * zz_multply;
                 per_histw(24) = pw_per24 * zz_multply;
                 per_histw(25) = pw_per25 * zz_multply;
                 per_histw(26) = pw_per26 * zz_multply;
                 per_histw(27) = pw_per27 * zz_multply;
                 per_histw(28) = pw_per28 * zz_multply;
                 per_histw(29) = pw_per29 * zz_multply;
                 per_histw(30) = pw_per30 * zz_multply;
                 per_histw(31) = pw_per31 * zz_multply;
                 per_histw(32) = pw_per32 * zz_multply;
                 per_histw(33) = pw_per33 * zz_multply;
                 per_histw(34) = pw_per34 * zz_multply;
                 per_histw(35) = pw_per35 * zz_multply;
                 per_histw(36) = pw_per36 * zz_multply;
                 per_histw(37) = pw_per37 * zz_multply;
                 per_histw(38) = pw_per38 * zz_multply;
                 per_histw(39) = pw_per39 * zz_multply;
                 per_histw(40) = pw_per40 * zz_multply;
                 per_histw(41) = pw_per41 * zz_multply;
                 per_histw(42) = pw_per42 * zz_multply;
                 per_histw(43) = pw_per43 * zz_multply;
                 per_histw(44) = pw_per44 * zz_multply;
                 per_histw(45) = pw_per45 * zz_multply;
                 per_histw(46) = pw_per46 * zz_multply;
                 per_histw(47) = pw_per47 * zz_multply;
                 per_histw(48) = pw_per48 * zz_multply;
                 per_histw(49) = pw_per49 * zz_multply;
                 per_histw(50) = pw_per50 * zz_multply;
                 per_histw(51) = pw_per51 * zz_multply;
                 per_histw(52) = pw_per52 * zz_multply;
                 xx_year = pw_year;
                 exsr $_hist_chg;
                 xx_year -= 1;
                 prodh52b_key.pw_comp = comp;
                 prodh52b_key.pw_locn = zz_frlocn;
                 prodh52b_key.pw_supl = zz_frsupl;
                 prodh52b_key.pw_suplsub = zz_frsub;
                 prodh52b_key.pw_prod = zz_frprod;
                 prodh52b_key.pw_forcint = zz_frforci;
                 prodh52b_key.pw_histype = zz_histype;
                 prodh52b_key.pw_year = xx_year;
                 setll %kds(prodh52b_key) k_prodh52b;
                 reade(n) %kds(prodh52b_key) k_prodh52b;
                 if (sz_frlocn = *blanks) and
                    (sz_tolocn = *blanks) and
                    %eof(k_prodh52b);
                    dou not %eof(k_prodh52b) or
                        %eof(k_locatnsa);
                        reade(n) (comp) k_locatnsa;
                        if not %eof(k_locatnsa);
                           zz_frlocn = lc_locn;
                           zz_tolocn = lc_locn;
                           if pr_forcint = 52;
                              setll (*hival) k_prodh52b;
                              prodh52b_key.pw_comp = comp;
                              prodh52b_key.pw_locn = zz_frlocn;
                              prodh52b_key.pw_supl = zz_frsupl;
                              prodh52b_key.pw_suplsub = zz_frsub;
                              prodh52b_key.pw_prod = zz_frprod;
                              prodh52b_key.pw_forcint = zz_frforci;
                              prodh52b_key.pw_histype = zz_histype;
                              setll %kds(prodh52b_key:7) k_prodh52b;
                              reade(n) %kds(prodh52b_key:7) k_prodh52b;
                           endif;
                           domany = *off;
                        endif;
                    enddo;
                    if %eof(k_locatnsa);
                       domany = *on;
                       zz_tolocn = *blanks;
                       zz_frlocn = *blanks;
                       zl_tolocn = 'All';
                       zl_frlocn = 'All';
                       leave;
                    endif;
                 else;
                    domany = *on;
                 endif;
             enddo;
             add = *off;
             replace = *off;
          endif;
       else;
          add = *off;
          replace = *off;
       endif;

       addmsg = *off;
       diff19 = *off;
       endsr;

       ///////////////////////////////////////////////// chg history

       begsr $_hist_chg;

       if add = *on;
          *in81 = *on;
       endif;
       if replace = *on;
          *in81 = *off;
       endif;

       if (sz_frlocn = *blanks) and
          (sz_tolocn = *blanks) or
          (sz_frlocn <> *blanks) and
          (sz_tolocn <> *blanks);
       // edit product entry
          exsr $_edt_prod;
          if (errors = *off) and
             (zz_frforci = zz_toforci);
             if (add = *on) or
                (replace = *on);
                if pr_forcint < 52;
                   prodhisb_key.ph_comp = comp;
                   prodhisb_key.ph_locn = zz_tolocn;
                   prodhisb_key.ph_supl = zz_tosupl;
                   prodhisb_key.ph_suplsub = zz_tosub;
                   prodhisb_key.ph_prod = zz_toprod;
                   prodhisb_key.ph_forcint = zz_toforci;
                   prodhisb_key.ph_histype = zz_histype;
                   prodhisb_key.ph_year = xx_year;
                   chain %kds(prodhisb_key) k_prodhisb;
                   if replace = *on;
                      ph_per01     = per_hist(01);
                      ph_per02     = per_hist(02);
                      ph_per03     = per_hist(03);
                      ph_per04     = per_hist(04);
                      ph_per05     = per_hist(05);
                      ph_per06     = per_hist(06);
                      ph_per07     = per_hist(07);
                      ph_per08     = per_hist(08);
                      ph_per09     = per_hist(09);
                      ph_per10     = per_hist(10);
                      ph_per11     = per_hist(11);
                      ph_per12     = per_hist(12);
                      ph_per13     = per_hist(13);
                   endif;
                   if add = *on and
                      *in03 = *on;
                       ph_per01     = per_hist(01);
                       ph_per02     = per_hist(02);
                       ph_per03     = per_hist(03);
                       ph_per04     = per_hist(04);
                       ph_per05     = per_hist(05);
                       ph_per06     = per_hist(06);
                       ph_per07     = per_hist(07);
                       ph_per08     = per_hist(08);
                       ph_per09     = per_hist(09);
                       ph_per10     = per_hist(10);
                       ph_per11     = per_hist(11);
                       ph_per12     = per_hist(12);
                       ph_per13     = per_hist(13);
                   endif;
                   if add = *on and
                      *in03 = *off;
                      ph_per01 = ph_per01 + per_hist(01);
                      ph_per02 = ph_per02 + per_hist(02);
                      ph_per03 = ph_per03 + per_hist(03);
                      ph_per04 = ph_per04 + per_hist(04);
                      ph_per05 = ph_per05 + per_hist(05);
                      ph_per06 = ph_per06 + per_hist(06);
                      ph_per07 = ph_per07 + per_hist(07);
                      ph_per08 = ph_per08 + per_hist(08);
                      ph_per09 = ph_per09 + per_hist(09);
                      ph_per10 = ph_per10 + per_hist(10);
                      ph_per11 = ph_per11 + per_hist(11);
                      ph_per12 = ph_per12 + per_hist(12);
                      ph_per13 = ph_per13 + per_hist(13);
                   endif;
                   if *in03 = *off;
                      ph_lastupd = lc_sysdate;
                      update rk_prodhis;
                   else;
                      ph_lastupd = lc_sysdate;
                      ph_birth   = lc_sysdate;
                      ph_comp    = comp;
                      ph_locn    = zz_tolocn;
                      ph_supl    = zz_tosupl;
                      ph_suplsub = zz_tosub;
                      ph_suplusr = pr_suplusr;
                      ph_suplusb = pr_suplusb;
                      ph_prod    = zz_toprod;
                      ph_year    = xx_year;
                      ph_forcint = zz_frforci;
                      write rk_prodhis;
                   endif;

                   if domany = *off;
                      callp K3S_Retrieve_Timestamp(time_stamp);
                      time = %time(%subst(%char(time_stamp):12:8):*iso);
                      date = %date(%subst(%char(time_stamp):1:10):*ISO);
                      chain (comp:zz_tolocn:zz_tosupl:zz_tosub:zz_toprod)
                            k_producta;
                      prodsoqb_key.pq_comp = comp;
                      prodsoqb_key.pq_buyr = zz_tobuyr;
                      prodsoqb_key.pq_locn = zz_tolocn;
                      prodsoqb_key.pq_supl = zz_tosupl;
                      prodsoqb_key.pq_suplsub = zz_tosub;
                      prodsoqb_key.pq_soqseq# = zz_toso#;
                      prodsoqb_key.pq_prod = zz_toprod;
                      chain %kds(prodsoqb_key) k_prodsoqb;
                      if not %found(k_prodsoqb);
                         prodsoqb_key.pq_soqseq# = zz_toso#1;
                         chain %kds(prodsoqb_key) k_prodsoqb;
                      endif;
                      suplsoqa_key.so_comp = comp;
                      suplsoqa_key.so_buyr = zz_tobuyr;
                      suplsoqa_key.so_locn = zz_tolocn;
                      suplsoqa_key.so_supl = zz_tosupl;
                      suplsoqa_key.so_suplsub = zz_tosub;
                      suplsoqa_key.so_soqseq# = zz_toso#;
                      chain %kds(suplsoqa_key) k_suplsoqa;
                      if not %found(k_suplsoqa);
                         suplsoqa_key.so_soqseq# = zz_toso#1;
                         chain %kds(suplsoqa_key) k_suplsoqa;
                      endif;
                      pf_comp = comp;
                      pf_locn = pr_locn;
                      pf_supl = pr_supl;
                      pf_suplsub = pr_suplsub;
                      pf_prod = pr_prod;
                      pf_chgtype = 'Y';
                      if replace = *on and
                         diff19 = *on;
                         pf_chgtype = 'Z';
                      endif;
                      if updtype = 'O';
                         pf_user = zz_user;
                         pf_workstn = wrk_statn;
                         pf_chgdesc = *blanks;
                         if add = *on;
                            pf_chgdesc = 'User add history    ';
                         endif;
                         if replace = *on;
                            pf_chgdesc = 'User replace history';
                         endif;
                      endif;
                      if updtype = 'B';
                         pf_user = 'NIGHT JOB ';
                         pf_workstn = 'NIGHT JOB ';
                         pf_chgdesc = *blanks;
                         if add = *on;
                            pf_chgdesc = 'Product link add    ';
                         endif;
                         if replace = *on;
                            pf_chgdesc = 'Product link replace';
                         endif;
                      endif;
                      pf_program = zz_program;
                      pf_birth = lc_sysdate;
                      pf_birthtm = time;
                      if add = *on;
                         zz_forcast = zz_forcast + pr_forcast;
                         if zz_frforci = zz_toforci and
                            zz_forcyr = xx_forcyr and
                            zz_forcper = xx_forcper or
                            zz_frforci <> zz_toforci;
                            zz_accsale += pr_accsale;
                            zz_accouts += pr_accouts;
                            zz_accdem  += pr_accdem;
                         endif;
                      endif;
                      pf_avgdiff = zz_forcast - pr_forcast;
                      pf_avgbef  = pr_forcast;
                      pf_avgaft  = zz_forcast;
                      pf_devpbef = pr_fordevp;
                      pf_devpaft = zz_fordevp;
                      pf_seasbef = pr_seasonl;
                      pf_seasaft = zz_seasonl;
                      pf_statbef = pr_usrstat;
                      pf_stataft = pr_usrstat;
                      pf_sysstat = pr_sysstat;
                      pr_seasonl = zz_seasonl;
                      if zz_seasonl <> *blanks;
                         pr_seassrc = 'U';
                         pr_seasact = lc_sysdate;
                      else;
                         pr_seassrc = ' ';
                         pr_seasact = %date('0001-01-01':*iso);
                      endif;
                      pr_fordevp = zz_fordevp;
                      pq_forcast = zz_forcast;
                      pr_forcast = zz_forcast;
                      pr_forserr = 0;
                      if zz_frforci = zz_toforci and
                         zz_forcyr = xx_forcyr and
                         zz_forcper = xx_forcper or
                         zz_frforci <> zz_toforci;
                         pr_accsale = zz_accsale;
                         pr_accouts = zz_accouts;
                      endif;
                      if zz_fstslyr < pr_fstslyr and
                         zz_fstslyr <> 0;
                         pr_fstslyr = zz_fstslyr;
                         pr_fstslpr = zz_fstslpr;
                      endif;
                      if (zz_fstslyr = pr_fstslyr) and
                         (zz_fstslpr < pr_fstslpr);
                         pr_fstslpr = zz_fstslpr;
                      endif;
                      if (pr_fstslyr = 0) and
                         (pr_fstslpr = 0);
                         pr_fstslyr = zz_fstslyr;
                         pr_fstslpr = zz_fstslpr;
                      endif;
                      pr_forchg = 'Y';
                      if replace = *on and
                         diff19 = *on;
                         pr_forchg = 'Z';
                      endif;
                      if zz_frforci = zz_toforci and
                         zz_forcyr = xx_forcyr and
                         zz_forcper = xx_forcper or
                         zz_frforci <> zz_toforci;
                         pr_accdem  = zz_accdem;
                      endif;
                      pr_formanl = lc_sysdate;
                      pr_lastupd = lc_sysdate;
       //********     if        pq_chkchg = 0
       //********     eval      pq_chkchg = 1
       //********     eval      so_chkchg = so_chkchg + 1
       //********     endif
                      *in80 = *on;
                      zl_frlocn  = zz_frlocn;
                      zl_frsupl  = zz_frsupl;
                      zl_frsub   = zz_frsub;
                      zl_frprod  = zz_frprod;
                      zl_frdesc1 = zz_frdesc1;
                      zl_tolocn  = zz_tolocn;
                      zl_tosupl  = zz_tosupl;
                      zl_tosub   = zz_tosub;
                      zl_toprod  = zz_toprod;
                      zl_todesc1 = zz_todesc1;
                      zl_multply = zz_multply;
                      pr_ansale$ = pr_forcast * pr_sales * pr_forcint;
                      pr_ansaleu = pr_forcast * pr_forcint;
       //             new products will become Regular (if forecast exists)
                      if pr_sysstat = 'N' and pr_forcast > 0;
                         pr_sysstat = 'R';
                         pq_chknew = 0;
                         so_chknew -= 1;
       //*************   if        pq_chkchg = 0
       //*************   eval      pq_chkchg = 1
       //*************   eval      so_chkchg = so_chkchg + 1
       //*************   endif
                      endif;
      /end-free
     d/copy k3s_c160
      /free
                      update rk_product;

                      clear savebirth;
                      chain (comp:zz_frlocn:zz_frsupl:zz_frsub:zz_frprod)
                            k_producta;
                      if %found (k_producta);
                         if pr_rfbirth <>  pr_birth or
                            pr_rfbirth <  pr_birth;
                            savebirth = pr_rfbirth;
                         endif;
                      endif;
                      chain (comp:zz_tolocn:zz_tosupl:zz_tosub:zz_toprod)
                            k_producta;

       //             if        savebirth <> '0001-01-01'
       //             eval      pr_rfbirth = savebirth
       //             update    rk_product
       //             endif

                      if %found(k_prodsoqb);
                         update rk_prodsoq;
                      endif;
                      if %found(k_suplsoqa);
                         update rk_suplsoq;
                      endif;
                      exsr $_wrt_prod;
                      exsr $_edt_stat;
                   endif;
                endif;

                if pr_forcint = 52;
                   prodh52b_key.pw_comp = comp;
                   prodh52b_key.pw_locn = zz_tolocn;
                   prodh52b_key.pw_supl = zz_tosupl;
                   prodh52b_key.pw_suplsub = zz_tosub;
                   prodh52b_key.pw_prod = zz_toprod;
                   prodh52b_key.pw_forcint = zz_toforci;
                   prodh52b_key.pw_histype = zz_histype;
                   prodh52b_key.pw_year = xx_year;
                   chain %kds(prodh52b_key) k_prodh52b;
                   if replace = *on;
                      pw_per01     = per_histw(01);
                      pw_per02     = per_histw(02);
                      pw_per03     = per_histw(03);
                      pw_per04     = per_histw(04);
                      pw_per05     = per_histw(05);
                      pw_per06     = per_histw(06);
                      pw_per07     = per_histw(07);
                      pw_per08     = per_histw(08);
                      pw_per09     = per_histw(09);
                      pw_per10     = per_histw(10);
                      pw_per11     = per_histw(11);
                      pw_per12     = per_histw(12);
                      pw_per13     = per_histw(13);
                      pw_per14     = per_histw(14);
                      pw_per15     = per_histw(15);
                      pw_per16     = per_histw(16);
                      pw_per17     = per_histw(17);
                      pw_per18     = per_histw(18);
                      pw_per19     = per_histw(19);
                      pw_per20     = per_histw(20);
                      pw_per21     = per_histw(21);
                      pw_per22     = per_histw(22);
                      pw_per23     = per_histw(23);
                      pw_per24     = per_histw(24);
                      pw_per25     = per_histw(25);
                      pw_per26     = per_histw(26);
                      pw_per27     = per_histw(27);
                      pw_per28     = per_histw(28);
                      pw_per29     = per_histw(29);
                      pw_per30     = per_histw(30);
                      pw_per31     = per_histw(31);
                      pw_per32     = per_histw(32);
                      pw_per33     = per_histw(33);
                      pw_per34     = per_histw(34);
                      pw_per35     = per_histw(35);
                      pw_per36     = per_histw(36);
                      pw_per37     = per_histw(37);
                      pw_per38     = per_histw(38);
                      pw_per39     = per_histw(39);
                      pw_per40     = per_histw(40);
                      pw_per41     = per_histw(41);
                      pw_per42     = per_histw(42);
                      pw_per43     = per_histw(43);
                      pw_per44     = per_histw(44);
                      pw_per45     = per_histw(45);
                      pw_per46     = per_histw(46);
                      pw_per47     = per_histw(47);
                      pw_per48     = per_histw(48);
                      pw_per49     = per_histw(49);
                      pw_per50     = per_histw(50);
                      pw_per51     = per_histw(51);
                      pw_per52     = per_histw(52);
                  endif;
                  if add = *on and
                     *in03 = *on;
                     pw_per01     = per_histw(01);
                     pw_per02     = per_histw(02);
                     pw_per03     = per_histw(03);
                     pw_per04     = per_histw(04);
                     pw_per05     = per_histw(05);
                     pw_per06     = per_histw(06);
                     pw_per07     = per_histw(07);
                     pw_per08     = per_histw(08);
                     pw_per09     = per_histw(09);
                     pw_per10     = per_histw(10);
                     pw_per11     = per_histw(11);
                     pw_per12     = per_histw(12);
                     pw_per13     = per_histw(13);
                     pw_per14     = per_histw(14);
                     pw_per15     = per_histw(15);
                     pw_per16     = per_histw(16);
                     pw_per17     = per_histw(17);
                     pw_per18     = per_histw(18);
                     pw_per19     = per_histw(19);
                     pw_per20     = per_histw(20);
                     pw_per21     = per_histw(21);
                     pw_per22     = per_histw(22);
                     pw_per23     = per_histw(23);
                     pw_per24     = per_histw(24);
                     pw_per25     = per_histw(25);
                     pw_per26     = per_histw(26);
                     pw_per27     = per_histw(27);
                     pw_per28     = per_histw(28);
                     pw_per29     = per_histw(29);
                     pw_per30     = per_histw(30);
                     pw_per31     = per_histw(31);
                     pw_per32     = per_histw(32);
                     pw_per33     = per_histw(33);
                     pw_per34     = per_histw(34);
                     pw_per35     = per_histw(35);
                     pw_per36     = per_histw(36);
                     pw_per37     = per_histw(37);
                     pw_per38     = per_histw(38);
                     pw_per39     = per_histw(39);
                     pw_per40     = per_histw(40);
                     pw_per41     = per_histw(41);
                     pw_per42     = per_histw(42);
                     pw_per43     = per_histw(43);
                     pw_per44     = per_histw(44);
                     pw_per45     = per_histw(45);
                     pw_per46     = per_histw(46);
                     pw_per47     = per_histw(47);
                     pw_per48     = per_histw(48);
                     pw_per49     = per_histw(49);
                     pw_per50     = per_histw(50);
                     pw_per51     = per_histw(51);
                     pw_per52     = per_histw(52);
                  endif;
                  if add = *on and
                     *in03 = *off;
                     pw_per01 += per_histw(01);
                     pw_per02 += per_histw(02);
                     pw_per03 += per_histw(03);
                     pw_per04 += per_histw(04);
                     pw_per05 += per_histw(05);
                     pw_per06 += per_histw(06);
                     pw_per07 += per_histw(07);
                     pw_per08 += per_histw(08);
                     pw_per09 += per_histw(09);
                     pw_per10 += per_histw(10);
                     pw_per11 += per_histw(11);
                     pw_per12 += per_histw(12);
                     pw_per13 += per_histw(13);
                     pw_per14 += per_histw(14);
                     pw_per15 += per_histw(15);
                     pw_per16 += per_histw(16);
                     pw_per17 += per_histw(17);
                     pw_per18 += per_histw(18);
                     pw_per19 += per_histw(19);
                     pw_per20 += per_histw(20);
                     pw_per21 += per_histw(21);
                     pw_per22 += per_histw(22);
                     pw_per23 += per_histw(23);
                     pw_per24 += per_histw(24);
                     pw_per25 += per_histw(25);
                     pw_per26 += per_histw(26);
                     pw_per27 += per_histw(27);
                     pw_per28 += per_histw(28);
                     pw_per29 += per_histw(29);
                     pw_per30 += per_histw(30);
                     pw_per31 += per_histw(31);
                     pw_per32 += per_histw(32);
                     pw_per33 += per_histw(33);
                     pw_per34 += per_histw(34);
                     pw_per35 += per_histw(35);
                     pw_per36 += per_histw(36);
                     pw_per37 += per_histw(37);
                     pw_per38 += per_histw(38);
                     pw_per39 += per_histw(39);
                     pw_per40 += per_histw(40);
                     pw_per41 += per_histw(41);
                     pw_per42 += per_histw(42);
                     pw_per43 += per_histw(43);
                     pw_per44 += per_histw(44);
                     pw_per45 += per_histw(45);
                     pw_per46 += per_histw(46);
                     pw_per47 += per_histw(47);
                     pw_per48 += per_histw(48);
                     pw_per49 += per_histw(49);
                     pw_per50 += per_histw(50);
                     pw_per51 += per_histw(51);
                     pw_per52 += per_histw(52);
                  endif;
                  if *in03 = *off;
                      pw_lastupd = lc_sysdate;
                      update  rk_prodh52;
                  else;
                      pw_lastupd = lc_sysdate;
                      pw_birth   = lc_sysdate;
                      pw_comp    = comp;
                      pw_locn    = zz_tolocn;
                      pw_supl    = zz_tosupl;
                      pw_suplsub = zz_tosub;
                      pw_suplusr = pr_suplusr;
                      pw_suplusb = pr_suplusb;
                      pw_prod    = zz_toprod;
                      pw_year    = xx_year;
                      pw_forcint = zz_frforci;
                      write rk_prodh52;
                  endif;
                  if domany = *off;
                     callp K3S_Retrieve_Timestamp(time_stamp);
                     time = %time(%subst(%char(time_stamp):12:8):*iso);
                     date = %date(%subst(%char(time_stamp):1:10):*ISO);
                     chain (comp:zz_tolocn:zz_tosupl:zz_tosub:zz_toprod)
                           k_producta;
                     prodsoqb_key.pq_comp = comp;
                     prodsoqb_key.pq_buyr = zz_tobuyr;
                     prodsoqb_key.pq_locn = zz_tolocn;
                     prodsoqb_key.pq_supl = zz_tosupl;
                     prodsoqb_key.pq_suplsub = zz_tosub;
                     prodsoqb_key.pq_soqseq# = zz_toso#;
                     prodsoqb_key.pq_prod = zz_toprod;
                     chain %kds(prodsoqb_key) k_prodsoqb;
                     if not %found(k_prodsoqb);
                        prodsoqb_key.pq_soqseq# = zz_toso#1;
                        chain %kds(prodsoqb_key) k_prodsoqb;
                     endif;
                     suplsoqa_key.so_comp = comp;
                     suplsoqa_key.so_buyr = zz_tobuyr;
                     suplsoqa_key.so_locn = zz_tolocn;
                     suplsoqa_key.so_supl = zz_tosupl;
                     suplsoqa_key.so_suplsub = zz_tosub;
                     suplsoqa_key.so_soqseq# = zz_toso#;
                     chain %kds(suplsoqa_key) k_suplsoqa;
                     if not %found(k_suplsoqa);
                        suplsoqa_key.so_soqseq# = zz_toso#1;
                        chain %kds(suplsoqa_key) k_suplsoqa;
                     endif;
                     pf_comp = comp;
                     pf_locn = pr_locn;
                     pf_supl = pr_supl;
                     pf_suplsub = pr_suplsub;
                     pf_prod = pr_prod;
                     pf_chgtype = 'Y';
                     if replace = *on and
                        diff19 = *on;
                        pf_chgtype = 'Z';
                     endif;
                     if updtype = 'O';
                        pf_user = zz_user;
                        pf_workstn = wrk_statn;
                        pf_chgdesc = *blanks;
                        if add = *on;
                           pf_chgdesc = 'User add history    ';
                        endif;
                        if replace = *on;
                           pf_chgdesc = 'User replace history';
                        endif;
                     endif;
                     if updtype = 'B';
                        pf_user = 'NIGHT JOB ';
                        pf_workstn = 'NIGHT JOB ';
                        pf_chgdesc = *blanks;
                        if add = *on;
                           pf_chgdesc = 'Product link add    ';
                        endif;
                        if replace = *on;
                           pf_chgdesc = 'Product link replace';
                        endif;
                     endif;
                     pf_program = zz_program;
                     pf_birth = lc_sysdate;
                     pf_birthtm = time;
                     if add = *on;
                        zz_forcast = zz_forcast + pr_forcast;
                        if zz_frforci = zz_toforci and
                           zz_forcyr = xx_forcyr and
                           zz_forcper = xx_forcper or
                           zz_frforci <> zz_toforci;
                           zz_accsale += pr_accsale;
                           zz_accouts += pr_accouts;
                           zz_accdem  += pr_accdem;
                        endif;
                     endif;
                     pf_avgdiff = zz_forcast - pr_forcast;
                     pf_avgbef  = pr_forcast;
                     pf_avgaft  = zz_forcast;
                     pf_devpbef = pr_fordevp;
                     pf_devpaft = zz_fordevp;
                     pf_seasbef = pr_seasonl;
                     pf_seasaft = zz_seasonl;
                     pf_statbef = pr_usrstat;
                     pf_stataft = pr_usrstat;
                     pf_sysstat = pr_sysstat;
                     pr_seasonl = zz_seasonl;
                     if zz_seasonl <> *blanks;
                        pr_seassrc = 'U';
                        pr_seasact = lc_sysdate;
                     else;
                        pr_seassrc = ' ';
                        pr_seasact = %date('0001-01-01':*iso);
                     endif;
                     pr_fordevp = zz_fordevp;
                     pq_forcast = zz_forcast;
                     pr_forcast = zz_forcast;
                     pr_forserr = 0;
                     if zz_frforci = zz_toforci and
                        zz_forcyr = xx_forcyr and
                        zz_forcper = xx_forcper or
                        zz_frforci <> zz_toforci;
                        pr_accsale = zz_accsale;
                        pr_accouts = zz_accouts;
                        pr_accdem  = zz_accdem;
                     endif;
                     if zz_fstslyr < pr_fstslyr and
                        zz_fstslyr <> 0;
                        pr_fstslyr = zz_fstslyr;
                        pr_fstslpr = zz_fstslpr;
                     endif;
                     if (zz_fstslyr = pr_fstslyr) and
                        (zz_fstslpr < pr_fstslpr);
                        pr_fstslpr = zz_fstslpr;
                     endif;
                     if (pr_fstslyr = 0) and
                        (pr_fstslpr = 0);
                        pr_fstslyr = zz_fstslyr;
                        pr_fstslpr = zz_fstslpr;
                     endif;
                     pr_formanl = lc_sysdate;
                     pr_forchg = 'Y';
                     if replace = *on and
                        diff19 = *on;
                        pr_forchg = 'Z';
                     endif;
                     pr_lastupd = lc_sysdate;
       //*********   if        pq_chkchg = 0
       //**********  eval      pq_chkchg = 1
       //**********  eval      so_chkchg = so_chkchg + 1
       //**********  endif
                     *in80 = *on;
                     zl_frlocn  = zz_frlocn;
                     zl_frsupl  = zz_frsupl;
                     zl_frsub   = zz_frsub;
                     zl_frprod  = zz_frprod;
                     zl_frdesc1 = zz_frdesc1;
                     zl_tolocn  = zz_tolocn;
                     zl_tosupl  = zz_tosupl;
                     zl_tosub   = zz_tosub;
                     zl_toprod  = zz_toprod;
                     zl_todesc1 = zz_todesc1;
                     zl_multply = zz_multply;
                     pr_ansale$ = pr_forcast * pr_sales *
                                  pr_forcint;
                     pr_ansaleu = pr_forcast * pr_forcint;
       // new products will become Regular (if forecast exists)
                     if pr_sysstat = 'N' and pr_forcast > 0;
                        pr_sysstat = 'R';
                        pq_chknew = 0;
                        so_chknew -= 1;
       //**********     if pq_chkchg = 0;
       //**********     eval      pq_chkchg = 1
       //**********     eval      so_chkchg = so_chkchg + 1
       //**********     endif
                     endif;
      /end-free
     d/copy k3s_c160
      /free
                     update rk_product;

                     clear savebirth;
                     producta_key.pr_comp = comp;
                     producta_key.pr_locn = zz_frlocn;
                     producta_key.pr_supl = zz_frsupl;
                     producta_key.pr_suplsub = zz_frsub;
                     producta_key.pr_prod = zz_frprod;
                     chain %kds(producta_key) k_producta;
                     if %found(k_producta);
                        if pr_rfbirth <>  pr_birth or
                           pr_rfbirth <  pr_birth;
                           savebirth = pr_rfbirth;
                        endif;
                     endif;

                     producta_key.pr_comp = comp;
                     producta_key.pr_locn = zz_tolocn;
                     producta_key.pr_supl = zz_tosupl;
                     producta_key.pr_suplsub = zz_tosub;
                     producta_key.pr_prod = zz_toprod;
                     chain %kds(producta_key) k_producta;

       //*****       if        savebirth <> '0001-01-01'
       //*****       eval      pr_rfbirth = savebirth
       //****        update    rk_product
       //****        endif

                     if %found(k_prodsoqb);
                        update rk_prodsoq;
                     endif;
                     if %found(k_suplsoqa);
                        update rk_suplsoq;
                     endif;
                     exsr $_wrt_prod;
                     exsr $_edt_stat;
                  endif;
                endif;
             endif;
          endif;
       endif;

       zz_modedsp = mode;
       *in98 = *off;
       if addmsg = *off;
          exsr $_add_msg;
          exsr $_pass_msg;
       endif;
       endsr;

       //////////////////////////////////////Call appropriate selection Pgm

       begsr $_cur_sel;

       if (*in(tablphtNOT) = *on) and
          (lda_window = 1) or
          (key_press = f04_key) and
          (zz_field = 'ZZ_HISTYPE');
          ta_codeval = %editc(zz_histype:'X');

       // call module to obtain table code file information for type
          ta_codetyp = 'PHT';
          blank = ' ';
          flag3_test = 0;
          callp K3S_9540(comp:
                         ta_codetyp:
                         ta_codeval:
                         send_error:
                         blank:
                         flag3_test);
          zz_histype = %dec(ta_codeval:1:0);
       endif;

       if (key_press = f04_key) and
          (zz_field = 'ZZ_FRPROD ');
          callp K3S_9560(comp:
                        zz_frsupl:
                        zz_frsub:
                        zz_frprod);
       endif;

       if (key_press = f04_key) and
          (zz_field = 'ZZ_TOPROD ');
          callp K3S_9560(comp:
                        zz_tosupl:
                        zz_tosub:
                        zz_toprod);
       endif;

       if (*in(locatnfNOT) = *on) and
          (lda_window = 1) or
          (key_press = f04_key) and
          (zz_field = 'ZZ_FRLOCN ');

       // call module to obtain location numbers and names.
          blank = ' ';
          callp K3S_9510(comp:
                         zz_frlocn:
                         send_error:
                         blank);
       endif;

       if (key_press = f04_key) and
          (zz_field = 'ZZ_FRSUPL ') or
          (key_press = f04_key) and
          (zz_field = 'ZZ_FRSUB  ');

       // call module to obtain supplier numbers and names.
          blank = ' ';
          callp K3S_9520(comp:
                        lda_buyr:
                        zz_frlocn:
                        xx_frsupl:
                        xx_frsub:
                        send_error:
                        blank);
          if xx_frsupl <> *blanks;
             zz_frsupl = xx_frsupl;
          endif;
          if xx_frsub  <> *blanks;
             zz_frsub  = xx_frsub;
          endif;
       endif;

       if (*in(locatntNOT) = *on) and
          (lda_window = 1) or
          (key_press = f04_key) and
          (zz_field = 'ZZ_TOLOCN ');

       // call module to obtain location numbers and names.
          blank = '1';
          callp K3S_9510(comp:
                        zz_tolocn:
                        send_error:
                        blank);
       endif;

       if (key_press = f04_key) and
          (zz_field = 'ZZ_TOSUPL ') or
          (key_press = f04_key) and
          (zz_field = 'ZZ_TOSUB  ');

       // call module to obtain supplier numbers and names.
          blank = ' ';
          callp K3S_9520(comp:
                        lda_buyr:
                        zz_tolocn:
                        xx_tosupl:
                        xx_tosub:
                        send_error:
                        blank);
          if xx_tosupl <> *blanks;
             zz_tosupl = xx_tosupl;
          endif;
          if xx_tosub  <> *blanks;
             zz_tosub  = xx_tosub;
          endif;
       endif;

       // user status
       if (*in(tablpsuNOT) = *on) and
          (lda_window = 1) or
          (key_press = f04_key) and
          (zz_field = 'ZZ_USRSTAT');

          ta_codeval = %trimr(zz_usrstat);
          ta_codetyp = 'PSU';
          blank = '1';
          flag3_test = 0;
          callp K3S_9540(comp:
                        ta_codetyp:
                        ta_codeval:
                        send_error:
                        blank:
                        flag3_test);
          zz_usrstat = %trimr(ta_codeval);
       endif;


       endsr;
       ////////////////////////////////////////////// Edit hist type

       begsr $_edt_type;

       // prime key list to read this company only
       ta_comp    = comp;
       ta_codetyp = 'PHT';
       ta_codeval = *blanks;
       ta_codeval = %editc(zz_histype:'X');
       *in(tablphtNOT) = *off;
       // get requested type record in table file
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;

       // requested type does not exist
       if not %found(k_tablcoda);
          *in(tablphtNOT) = *on;
          errors = *on;
          *in62 = *on;
          *in61 = *off;
          send_error= *on;
          @msg_id = 'K3_9020';
          @msg_text = *blanks;
          @msg_text = %editc(zz_histype:'X');
          if addmsg = *off;
             exsr $_add_msg;
          endif;

       else;
          zz_histdsc = ta_codeds1;
       endif;

       endsr;
       //////////////////////////////////////////////// Edit user status

       begsr $_edt_sta1;

       // prime key list to read this company only
       ta_comp    = comp;
       ta_codetyp = 'PSU';
       ta_codeval = zz_usrstat;

       // blank requested type would mean all types
       if zz_usrstat = ' ' and
          zz_frezend <> *blanks;
          errors = *on;
          *in29 = *on;
          @msg_id   = 'K3_9999';
          @msg_text = 'Freeze Until MUST be blank     ';
          exsr $_add_msg;
       endif;
       if zz_usrstat = *blanks;

       // otherwise, validate entry
       else;

       // get requested type record in table file
          chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;
          *in(tablpsuNOT) = *off;
       // requested type does not exist
          if not %found(k_tablcoda);
             *in(tablpsuNOT) = *on;
             errors    = *on;
             send_error= *on;
             @msg_id   = 'K3_9360';
             @msg_text = zz_usrstat;
             exsr $_add_msg;

          endif;

          if errors = *off and
             zz_usrstat = 'F' and
             zz_frezend = *blanks;
             errors = *on;
             *in29 = *on;
             @msg_id = 'K3_9999';
             @msg_text = 'Freeze Until MUST be entered   ';
             exsr $_add_msg;

          endif;

          if errors = *off and
             zz_usrstat <> 'F' and
             zz_frezend <> *blanks;
             errors = *on;
             *in29 = *on;
             @msg_id   = 'K3_9999';
             @msg_text = 'Freeze Until MUST be blank     ';
             exsr $_add_msg;

          endif;

          if errors = *off and
             zz_usrstat = 'P';
             errors = *on;
             *in30 = *on;
             @msg_id   = 'K3_9999';
             @msg_text = 'P status is invalid on copy    ';
             exsr $_add_msg;
          else;
             *in30 = *off;
          endif;

       endif;
       endsr;

       //////////////////////////////////////////////// Edit user status

       begsr $_edt_stat;

       if zz_usrstat <> *blanks or
          zz_setfore = '1';
          callp K3S_Retrieve_Timestamp(time_stamp);
          time = %time(%subst(%char(time_stamp):12:8):*iso);
          date = %date(%subst(%char(time_stamp):1:10):*iso);
          producta_key.pr_comp = comp;
          producta_key.pr_locn = zz_frlocn;
          producta_key.pr_supl = zz_frsupl;
          producta_key.pr_suplsub = zz_frsub;
          producta_key.pr_prod = zz_frprod;
          chain %kds(producta_key) k_producta;
          prodsoqb_key.pq_comp = comp;
          prodsoqb_key.pq_buyr = zz_frbuyr;
          prodsoqb_key.pq_locn = zz_frlocn;
          prodsoqb_key.pq_supl = zz_frsupl;
          prodsoqb_key.pq_suplsub = zz_frsub;
          prodsoqb_key.pq_soqseq# = zz_frso#;
          prodsoqb_key.pq_prod = zz_frprod;
          chain %kds(prodsoqb_key) k_prodsoqb;
          suplsoqa_key.so_comp = comp;
          suplsoqa_key.so_buyr = zz_frbuyr;
          suplsoqa_key.so_locn = zz_frlocn;
          suplsoqa_key.so_supl = zz_frsupl;
          suplsoqa_key.so_suplsub = zz_frsub;
          suplsoqa_key.so_soqseq# = zz_frso#;
          chain %kds(suplsoqa_key) k_suplsoqa;
          pf_comp = comp;
          pf_locn = pr_locn;
          pf_supl = pr_supl;
          pf_suplsub = pr_suplsub;
          pf_prod = pr_prod;
          pf_chgtype = 'Y';
          if replace = *on and
             diff19 = *on;
             pf_chgtype = 'Z';
          endif;
          if updtype = 'O';
             pf_user = zz_user;
             pf_workstn = wrk_statn;
             pf_chgdesc = *blanks;
             if add = *on;
                pf_chgdesc = 'User add history    ';
             endif;
             if replace = *on;
                pf_chgdesc = 'User replace history';
             endif;
          endif;
          if updtype = 'B';
             pf_user = 'NIGHT JOB ';
             pf_workstn = 'NIGHT JOB ';
             pf_chgdesc = *blanks;
             if add = *on;
                pf_chgdesc = 'Product link add    ';
             endif;
             if replace = *on;
                pf_chgdesc = 'Product link replace';
             endif;
          endif;
          pf_program = zz_program;
          pf_birth = lc_sysdate;
          pf_birthtm = time;
          if zz_setfore = '1';
             zz_forcast = 0;
          endif;
          if zz_setfore = '1';
             pr_forchg = 'Y';
          endif;
          if replace = *on and
            diff19 = *on;
            pr_forchg = 'Z';
          endif;
          pr_lastupd = lc_sysdate;
          if zz_setfore = '1' and
             zz_usrstat = *blanks;
       //*************      if        pq_chkchg = 0
       //*************      eval      pq_chkchg = 1
       //*************      eval      so_chkchg = so_chkchg + 1
       //*************      endif
          endif;
          if updtype = 'O';
             pf_user = zz_user;
             pf_workstn = wrk_statn;
          endif;
          if updtype = 'B';
             pf_user = 'NIGHT JOB ';
             pf_workstn = 'NIGHT JOB ';
          endif;
          pf_program = zz_program;
          if zz_setfore = '1';
             pf_avgdiff = zz_forcast - pr_forcast;
             pf_avgbef  = pr_forcast;
             pr_forcast = zz_forcast;
             pr_forserr = 0;
             pf_avgaft  = zz_forcast;
             pf_devpbef = pr_fordevp;
             pf_devpaft = pr_fordevp;
             pf_comp = comp;
             pf_locn = pr_locn;
             pf_supl = pr_supl;
             pf_suplsub = pr_suplsub;
             pf_seasbef = pr_seasonl;
             pf_seasaft = pr_seasonl;
             pf_prod = pr_prod;
             pf_chgtype = 'U';
             pf_statbef = pr_usrstat;
             pf_stataft = pr_usrstat;
             pf_sysstat = pr_sysstat;
             exsr $_wrt_prod;
          endif;
          if pr_usrstat <> zz_usrstat and
             zz_usrstat <> ' ';
             pf_birthtm = pf_birthtm + %seconds(1);
             pf_avgdiff = 0;
             pf_avgbef  = pr_forcast;
             pf_avgaft  = pr_forcast;
             pf_devpbef = pr_fordevp;
             pf_devpaft = pr_fordevp;
             pf_comp = comp;
             pf_locn = pr_locn;
             pf_supl = pr_supl;
             pf_suplsub = pr_suplsub;
             pf_seasbef = pr_seasonl;
             pf_seasaft = pr_seasonl;
             pf_prod = pr_prod;
             pf_chgtype = 'S';
             pf_statbef = pr_usrstat;
             pf_stataft = zz_usrstat;
             pf_sysstat = pr_sysstat;
             exsr $_wrt_prod;
          endif;
          if (pr_usrstat = 'M') and
             (zz_usrstat = 'W');
             pr_forchg = 'S';
             pq_chkmanl -= 1;
             pq_chkwatc += 1;
             pq_usrstat = zz_usrstat;
             so_chkmanl -= 1;
             so_chkwatc += 1;
       //*** if        pq_chkchg = 0
       //*** eval      pq_chkchg = 1
       //*** eval      so_chkchg = so_chkchg + 1
       //*** endif;
          endif;

          if (pr_usrstat = 'W') and
             (zz_usrstat = 'M');
             pr_forchg = 'S';
             pq_chkmanl += 1;
             pq_chkwatc -= 1;
             pq_usrstat = zz_usrstat;
             so_chkmanl += 1;
             so_chkwatc -= 1;
       //*** if pq_chkchg = 0;
       //*** eval      pq_chkchg = 1
       //*** eval      so_chkchg = so_chkchg + 1
       //*** endif
          endif;

          if (pr_usrstat = ' ') and
             (zz_usrstat = 'W');
             pr_forchg = 'S';
             pq_chkwatc += 1;
             pq_usrstat = zz_usrstat;
             so_chkwatc += 1;
       //*** if        pq_chkchg = 0
       //*** eval      pq_chkchg = 1
       //*** eval      so_chkchg = so_chkchg + 1
       //*** endif
          endif;

          if (pr_usrstat = ' ') and
             (zz_usrstat = 'M');
             pr_forchg = 'S';
             pq_chkmanl += 1;
             pq_usrstat = zz_usrstat;
             so_chkmanl += 1;
       //*** if        pq_chkchg = 0
       //*** eval      pq_chkchg = 1
       //*** eval      so_chkchg = so_chkchg + 1
       //*** endif
          endif;

          if (pr_usrstat = 'P') and
             (zz_usrstat = 'W');
             pq_chkwatc += 1;
             pq_chkprob -= 1;
             pq_usrstat = zz_usrstat;
             so_chkwatc += 1;
             so_chkprob -= 1;
       // if        pq_chkchg = 0
       // eval      pq_chkchg = 1
       // eval      so_chkchg = so_chkchg + 1
       // endif
             pr_probdat = %date('0001-01-01':*iso);
             pr_forchg = 'S';
          endif;

          if (pr_usrstat = 'P') and
             (zz_usrstat = 'M');
             pq_chkmanl += 1;
             pq_chkprob -= 1;
             pq_usrstat = zz_usrstat;
             so_chkmanl += 1;
             so_chkprob -= 1;
       // if        pq_chkchg = 0
       // eval      pq_chkchg = 1
       // eval      so_chkchg = so_chkchg + 1
       // endif
             pr_probdat = %date('0001-01-01':*iso);
             pr_forchg = 'S';
          endif;


          if (pr_usrstat = 'W') and
             (zz_usrstat = 'F');
             pq_chkwatc -= 1;
             pq_chkfrez += 1;
             pq_usrstat = zz_usrstat;
             so_chkwatc -= 1;
             so_chkfrez += 1;
       // if        pq_chkchg = 0
       // eval      pq_chkchg = 1
       // eval      so_chkchg = so_chkchg + 1
       // endif
             pr_forchg = 'S';
             pr_forfrez = iso_end;
          endif;

          if (pr_usrstat = 'P') and
             (zz_usrstat = 'F');
             pq_chkprob -= 1;
             pq_chkfrez += 1;
             pq_usrstat = zz_usrstat;
             so_chkprob -= 1;
             so_chkfrez += 1;
       // if        pq_chkchg = 0
       // eval      pq_chkchg = 1
       // eval      so_chkchg = so_chkchg + 1
       // endif
             pr_probdat = %date('0001-01-01':*iso);
             pr_forfrez = iso_end;
             pr_forchg = 'S';
          endif;

          if (pr_usrstat = 'M') and
             (zz_usrstat = 'F');
             pq_chkmanl -= 1;
             pq_chkfrez += 1;
             pq_usrstat = zz_usrstat;
             so_chkmanl -= 1;
             so_chkfrez += 1;
       // if        pq_chkchg = 0
       // eval      pq_chkchg = 1
       // eval      so_chkchg = so_chkchg + 1
       // endif
             pr_forchg = 'S';
             pr_forfrez = iso_end;
          endif;

          if (pr_usrstat = 'F') and
             (zz_usrstat = 'W');
             pq_chkwatc += 1;
             pq_chkfrez -= 1;
             pq_usrstat = zz_usrstat;
             so_chkwatc += 1;
             so_chkfrez -= 1;
       // if        pq_chkchg = 0
       // eval      pq_chkchg = 1
       // eval      so_chkchg = so_chkchg + 1
       // endif
             pr_forfrez = %date('0001-01-01':*iso);
             pr_forchg = 'S';
          endif;

          if (pr_usrstat = 'F') and
             (zz_usrstat = 'M');
             pq_chkmanl += 1;
             pq_chkfrez -= 1;
             pq_usrstat = zz_usrstat;
             so_chkmanl += 1;
             so_chkfrez -= 1;
       // if        pq_chkchg = 0
       // eval      pq_chkchg = 1
       // eval      so_chkchg = so_chkchg + 1
       // endif
             pr_forfrez = %date('0001-01-01':*iso);
             pr_forchg = 'S';
          endif;


          if (pr_usrstat = ' ') and
             (zz_usrstat = 'F');
             pq_chkfrez += 1;
             so_chkfrez += 1;
             pq_usrstat = zz_usrstat;
       // if        pq_chkchg = 0
       // eval      pq_chkchg = 1
       // eval      so_chkchg = so_chkchg + 1
       // endif
             pr_forfrez = iso_end;
             pr_forchg = 'S';
          endif;

          if zz_usrstat <> *blanks and
             zz_usrstat <> 'P';
             pr_usrstat = zz_usrstat;
          endif;

          update rk_product;
          if %found(k_prodsoqb);
             update rk_prodsoq;
          endif;
          if %found(k_suplsoqa);
             update rk_suplsoq;
          endif;

       endif;
       endsr;

       //////////////////////////////////////////////// Write k_prodfor

       begsr $_wrt_prod;

       callp K3S_3041(comp:
                      pf_locn:
                      pf_supl:
                      pf_suplsub:
                      pf_prod:
                      pf_birth:
                      pf_birthtm:
                      pf_chgtype:
                      pf_chgdesc:
                      pf_avgbef:
                      pf_avgaft:
                      pf_avgdiff:
                      pf_seasbef:
                      pf_seasaft:
                      pf_devpbef:
                      pf_devpaft:
                      pf_statbef:
                      pf_stataft:
                      pf_sysstat:
                      pf_user:
                      pf_workstn:
                      pf_program);
       endsr;

       /////////////////////////////////////////////////////// Edit end date

       begsr $_edt_endz;

       // edit requested end date
       errors = *off;
       clear iso_end;
       // call module to convert user entered date into *ISO format
       iso_error = 0;
       callp K3S_Date_To_ISO(zz_frezend:
                             lda_usrdat:
                             lda_date6:
                             iso_end:
                             iso_error);

       // requested end date entered, not valid
       if iso_error = 1;
          errors = *on;
          *in29 = *on;
          @msg_id = 'K3_9040';
          @msg_text = zz_frezend;
          exsr $_add_msg;
       else;
          *in29 = *off;
       endif;


       endsr;

       ///////////////////////////////////////////////// new history

       begsr $_fix_hist;

       if errors = *off;
          if zz_toforci < 52;
             if (sz_frlocn = *blanks) and
                (sz_tolocn = *blanks);
                setll (xx_comp) k_locatnsa;
                read(n) k_locatnsa;
                setll (comp) k_locatnsa;
                reade(n) (comp) rk_locatns;
                if not %eof(k_locatnsa);
                   zz_tolocn = lc_locn;
                endif;
             endif;

             setll *hival k_prodhisb;
             prodhisb_key.ph_comp = comp;
             prodhisb_key.ph_locn = zz_tolocn;
             prodhisb_key.ph_supl = zz_tosupl;
             prodhisb_key.ph_suplsub = zz_tosub;
             prodhisb_key.ph_prod = zz_toprod;
             prodhisb_key.ph_forcint = zz_toforci;
             prodhisb_key.ph_histype = zz_histype;
             setll %kds(prodhisb_key:7) k_prodhisb;
             reade %kds(prodhisb_key:7) k_prodhisb;

             dow not %eof(k_prodhisb);

                 delete rk_prodhis;
                 reade %kds(prodhisb_key:7) k_prodhisb;
                 if (sz_frlocn = *blanks) and
                    (sz_tolocn = *blanks) and
                    %eof(k_prodhisb);
                    reade(n) (comp) k_locatnsa;
                    if not %eof(k_locatnsa);
                       zz_tolocn = lc_locn;
                       prodhisb_key.ph_locn = zz_tolocn;
                       setll *hival k_prodhisb;
                       setll %kds(prodhisb_key:7) k_prodhisb;
                       reade %kds(prodhisb_key:7) k_prodhisb;
                    endif;
                 endif;
             enddo;
          endif;
          if zz_toforci = 52;
             if (sz_frlocn = *blanks) and
                (sz_tolocn = *blanks);
                setll (xx_comp) k_locatnsa;
                read(n) k_locatnsa;
                setll (comp) k_locatnsa;
                reade(n) (comp) k_locatnsa;
                if not %eof(k_locatnsa);
                   zz_tolocn = lc_locn;
                endif;
             endif;
             setll *hival k_prodh52b;
             prodh52b_key.pw_comp = comp;
             prodh52b_key.pw_locn = zz_tolocn;
             prodh52b_key.pw_supl = zz_tosupl;
             prodh52b_key.pw_suplsub = zz_tosub;
             prodh52b_key.pw_prod = zz_toprod;
             prodh52b_key.pw_forcint = zz_toforci;
             prodh52b_key.pw_histype = zz_histype;
             setll %kds(prodh52b_key:7) k_prodh52b;
             reade %kds(prodh52b_key:7) k_prodh52b;

             dow not %eof(k_prodh52b);

                 delete rk_prodh52;
                 reade %kds(prodh52b_key:7) k_prodh52b;
                 if (sz_frlocn = *blanks) and
                    (sz_tolocn = *blanks) and
                    %eof(k_prodh52b);
                    reade(n) (comp:locn) k_locatnsa;
                    if not %eof(k_locatnsa);
                       zz_tolocn = lc_locn;
                       prodh52b_key.pw_locn = zz_tolocn;
                       setll *hival k_prodh52b;
                       setll %kds(prodh52b_key:7) k_prodh52b;
                       reade %kds(prodh52b_key:7) k_prodh52b;
                    endif;
                 endif;
             enddo;
          endif;

          if zz_frforci < 52;
             if (sz_frlocn = *blanks) and
                (sz_tolocn = *blanks);
                setll (xx_comp) k_locatnsa;
                read(n) k_locatnsa;
                setll (comp) k_locatnsa;
                reade(n) (comp) k_locatnsa;
                if not %eof(k_locatnsa);
                   zz_tolocn = lc_locn;
                   zz_frlocn = lc_locn;
                endif;
             endif;
             setll *hival k_prodhisb;
             prodhisb_key.ph_comp = comp;
             prodhisb_key.ph_locn = zz_frlocn;
             prodhisb_key.ph_supl = zz_frsupl;
             prodhisb_key.ph_suplsub = zz_frsub;
             prodhisb_key.ph_prod = zz_frprod;
             prodhisb_key.ph_forcint = zz_frforci;
             prodhisb_key.ph_histype = zz_histype;
             setll %kds(prodhisb_key:7) k_prodhisb;
             reade(n) %kds(prodhisb_key:7) k_prodhisb;

             dow not %eof(k_prodhisb);

                 prodhisb_key.ph_comp = ph_comp;
                 prodhisb_key.ph_locn = ph_locn;
                 prodhisb_key.ph_supl = zz_tosupl;
                 prodhisb_key.ph_suplsub = zz_tosub;
                 prodhisb_key.ph_prod = zz_toprod;
                 prodhisb_key.ph_forcint = ph_forcint;
                 prodhisb_key.ph_histype = ph_histype;
                 prodhisb_key.ph_year = ph_year;

                 ph_supl = zz_tosupl;
                 ph_suplsub = zz_tosub;
                 ph_suplusr = pr_suplusr;
                 ph_suplusb = pr_suplusb;
                 ph_prod = zz_toprod;
                 chain %kds(prodhisb_key) k_prodhisb;
                 if not %found(k_prodhisb);
                    write rk_prodhis;
                 endif;
                 chain(n) (comp:zz_tolocn:zz_tosupl:zz_tosub:zz_toprod)
                          k_producta;
                 if %found(k_producta);
                    chain(n) (comp:zz_tolocn:zz_tosupl:zz_tosub:zz_toprod)
                             k_producta;
                    pr_forcint = zz_frforci;
                    pr_forcyr  = xx_forcyr;
                    pr_forcper = xx_forcper;
                    update rk_product;
                 endif;
                 prodhisb_key.ph_comp = comp;
                 prodhisb_key.ph_locn = zz_frlocn;
                 prodhisb_key.ph_supl = zz_frsupl;
                 prodhisb_key.ph_suplsub = zz_frsub;
                 prodhisb_key.ph_prod = zz_frprod;
                 prodhisb_key.ph_forcint = zz_frforci;
                 prodhisb_key.ph_histype = zz_histype;
                 reade(n) %kds(prodhisb_key:7) k_prodhisb;
                 if (sz_frlocn = *blanks) and
                    (sz_tolocn = *blanks) and
                    %eof(k_prodhisb);
                    reade(n) (comp) k_locatnsa;
                    if not %eof(k_locatnsa);
                       zz_tolocn = lc_locn;
                       zz_frlocn = lc_locn;
                       prodhisb_key.ph_comp = comp;
                       prodhisb_key.ph_locn = zz_frlocn;
                       prodhisb_key.ph_supl = zz_frsupl;
                       prodhisb_key.ph_suplsub = zz_frsub;
                       prodhisb_key.ph_prod = zz_frprod;
                       prodhisb_key.ph_forcint = zz_frforci;
                       prodhisb_key.ph_histype = zz_histype;
                       setll %kds(prodhisb_key:7) k_prodhisb;
                       reade(n) %kds(prodhisb_key:7) k_prodhisb;
                     endif;
                 endif;
             enddo;
          endif;

          if zz_frforci = 52;
             if (sz_frlocn = *blanks) and
                (sz_tolocn = *blanks);
                setll (xx_comp) k_locatnsa;
                read(n) k_locatnsa;
                setll (comp) k_locatnsa;
                reade(n) (comp) k_locatnsa;
                if not %eof(k_locatnsa);
                   zz_tolocn = lc_locn;
                   zz_frlocn = lc_locn;
                endif;
             endif;

             setll *hival k_prodh52b;
             setll (comp:zz_frlocn:zz_frsupl:zz_frsub:zz_frprod)
                       k_prodh52b;
             reade(n) (comp:zz_frlocn:zz_frsupl:zz_frsub:zz_frprod)
                       k_prodh52b;

             dow not %eof(k_prodh52b);

                 pw_supl = zz_tosupl;
                 pw_suplsub = zz_tosub;
                 pw_suplusr = pr_suplusr;
                 pw_suplusb = pr_suplusb;
                 pw_prod = zz_toprod;
                 prodh52b_key.pw_comp = pw_comp;
                 prodh52b_key.pw_locn = pw_locn;
                 prodh52b_key.pw_supl = pw_supl;
                 prodh52b_key.pw_suplsub = pw_suplsub;
                 prodh52b_key.pw_prod = pw_prod;
                 prodh52b_key.pw_forcint = pw_forcint;
                 prodh52b_key.pw_histype = pw_histype;
                 prodh52b_key.pw_year = pw_year;
                 chain %kds(prodh52b_key) k_prodh52b;
                 if not %found(k_prodh52b);
                    write rk_prodh52;
                 endif;
                 chain(n) (comp:zz_tolocn:zz_tosupl:zz_tosub:zz_toprod)
                          k_producta;
                 if %found(k_producta);
                    chain(n) (comp:zz_tolocn:zz_tosupl:zz_tosub:zz_toprod)
                              k_producta;
                    pr_forcint = zz_frforci;
                    pr_forcyr  = xx_forcyr;
                    pr_forcper = xx_forcper;
                    update rk_product;
                 endif;
                 reade(n) (comp:zz_frlocn:zz_frsupl:zz_frsub:zz_frprod)
                          k_prodh52b;
                 if (sz_frlocn = *blanks) and
                    (sz_tolocn = *blanks) and
                    %eof(k_prodh52b);
                    reade(n) (comp) k_locatnsa;
                    if not %eof(k_locatnsa);
                       zz_tolocn = lc_locn;
                       zz_frlocn = lc_locn;
                       setll *hival k_prodh52b;
                       setll (comp:zz_frlocn:zz_frsupl:zz_frsub:zz_frprod)
                          k_prodh52b;
                       reade(n) (comp:zz_frlocn:zz_frsupl:zz_frsub:zz_frprod)
                                k_prodh52b;
                    endif;
                 endif;
             enddo;
          endif;
       endif;

       if (sz_frlocn = *blanks) and
          (sz_tolocn = *blanks);
          zz_tolocn = sz_tolocn;
          zz_frlocn = sz_frlocn;
       endif;
       endsr;

       ///////////////////////////////////////////////// CALL K3S_3013

       begsr $_combine;

       callp K3S_3013(pr_comp:
                      pr_prod:
                      pr_locn:
                      pr_supl:
                      pr_suplsub:
                      pr_lastupd);
       endsr;

       begsr $_usr_exit;

       // prime F8=User exit text
       ta_comp    = lda_comp;
       ta_codetyp = 'UEX';
       ta_codeval = zz_program;
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;
       if %found(k_tablcoda);
          zz_fkey_8 = 'F8=' + ta_codeds1;
       else;
          zz_fkey_8 = @user_exit;
       endif;

       endsr;

       begsr $_get_titl;

       // get title of screen
       clear @blanks;
       ta_comp    = lda_comp;
       ta_codetyp = 'TLE';
       ta_codeval = %trimr(zz_program) + 'FM' +
                    @recd_frmt;

       // does title record exist?
       chain(n) (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;

       // title record does exist
       if %found(k_tablcoda);

       // determine title length, and starting position for centering
          @length = %checkr(' ':ta_codeds1);
          @start = (50 - @length) / 2;

       // center title
          zz_title = %subst(@blanks:1:(@start)) +
                     ta_codeds1;

       // title record does not exist, so provide message
       else;
          zz_title = @no_title;
       endif;

       endsr;

       //////////////////////////////////////////////////////////////////////
      /end-free

      * ---------------------------------- Message logic for screen programs
     c/copy k3s_c051

      * -----------------------------------------Cursor posistion subroutine
     c/copy k3s_c080

      * -----------------------------------------$_get_lda  subroutine
     c/copy k3s_c031

      * -----------------------------------------$_get_time subroutine
     c/copy k3s_c180

      * -----------------------------------------------$_usr_exit subroutine
      *c/copy k3s_c190

      * -----------------------------------------------$_f08_exit subroutine
     c/copy k3s_c191

      * -----------------------------------------------$_get_titl subroutine
      *c/copy k3s_c200

