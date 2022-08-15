      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

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
      **   Name: K3S_3433
      **   Type: ILE RPG Program
      **   Desc: Weekly distribution build selection criteria batch
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/9/2014.
      **  Remarks. Altered program to utilize SQL select statements to
      **           access K_ COMPANY and K_TABLCOD records.
      **
      *****************************************************************
      * --------------------------------------------------------- Workfields

     d buyr            s              5a   inz('     ')                         buy group
     d regn            s              5a   inz('     ')                         region
     d locn            s              5a   inz('     ')                         location
     d supl            s             10a   inz('          ')                    supplier
     d suplsub         s             10a   inz('          ')                    sub supplier
     d suplgp1         s             10a   inz('          ')                    supplier
     d suplgp2         s             10a   inz('          ')                    supplier
     d suplgp3         s             10a   inz('          ')                    supplier
     d suplgp4         s             10a   inz('          ')                    supplier
     d suplgp5         s             10a   inz('          ')                    supplier
     d prodgp1         s             10a   inz('          ')                    supplier
     d prodgp2         s             10a   inz('          ')                    supplier
     d prodgp3         s             10a   inz('          ')                    supplier
     d prodgp4         s             10a   inz('          ')                    supplier
     d prodgp5         s             10a   inz('          ')                    supplier
     d save_prt        s             10a   inz('QSYSPRT')
     d save_cpy        s              2a   inz('01')                            returned date frmtd
     d save_ufro       s              1a   inz('1')                             returned date frmtd
     d save_uman       s              1a   inz('1')                             returned date frmtd
     d save_upro       s              1a   inz('1')                             returned date frmtd
     d save_uwat       s              1a   inz('1')                             returned date frmtd
     d save_sdis       s              1a   inz('1')                             returned date frmtd
     d save_slum       s              1a   inz('1')                             returned date frmtd
     d save_snew       s              1a   inz('1')                             returned date frmtd
     d save_sreg       s              1a   inz('1')                             returned date frmtd
     d save_sslo       s              1a   inz('1')                             returned date frmtd
     d save_ublk       s              1a   inz('1')                             returned date frmtd
     d sequence        s              1a   inz('1')                             returned date frmtd
     d daynbr          s              1p 0                                      # valid locations
     d weeks           s              7  0                                      # valid locations
     d xx_begin        s               d   datfmt(*iso)                         batch begin date
     d xx_end          s               d   datfmt(*iso)                         batch end date
     d cmsysdate       s               d
     ddatestruc1       ds
     dwrkdate                         6s 0
     d #month                         2s 0 overlay(wrkdate)
     d #day                           2s 0 overlay(wrkdate:3)
     d #year                          2s 0 overlay(wrkdate:5)
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d tablcod_rec   e ds                  ExtName(k_tablcod)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3433        PI
     d  comp                          1
      * ---------------------------------------------------
      /free
       exec sql
       set option commit = *none,
                  datfmt = *iso,
                  closqlcsr = *endactgrp;
       //once routine
       exsr $_once;

       //finished, set on LR
       *inlr = *on;

       //**************************************************** End of program

       //////////////////////////////////////////////////////// Once routine

       begsr $_once;

       exec sql
         select *
           into :tablcod_rec
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'K3S_3920  COLLECT   '
           fetch first row only;
       if SQLState = SQLStateOk
          and ta_flag1 = 1;

          exec sql
            select *
              into :tablcod_rec
              from k_tablcod
              where ta_comp = :comp and
                    ta_codetyp = 'APP' and
                    ta_codeval = 'K3S_3433  COLLECT   '
                    fetch first row only;
          if SQLState = SQLStateOk
             and ta_flag1 = 1;

             exec sql
               select cm_sysdate
                 into :cmsysdate
                 from k_company
                 where cm_comp = :comp
                 fetch first row only;

             xx_end = cmsysdate;

             if ta_flag2 = 1;
       //  find sunday
                 weeks = %diff(xx_end:d'1900-12-30':*days);
                 daynbr = %rem(weeks:7);
                 if daynbr = 0;
                    xx_begin = xx_end   - %days(6);
                    xx_end   = xx_begin + %days(6);
                 endif;
             endif;

             if ta_flag2 = 2;
       //  find monday
                weeks = %diff(xx_end:d'1900-12-31':*days);
                daynbr = %rem(weeks:7);
                if daynbr = 0;
                   xx_begin = xx_end - %days(7);
                   xx_end   = xx_begin + %days(6);
                endif;
             endif;

             if ta_flag2 = 3;
       //  find tuesday
                weeks = %diff(xx_end:d'1901-01-01':*days);
                daynbr = %rem(weeks:7);
                if daynbr = 0;
                   xx_begin = xx_end - %days(8);
                   xx_end   = xx_begin + %days(6);
                endif;
             endif;

             if ta_flag2 = 4;
       //  find wednesday
                weeks = %diff(xx_end:d'1901-01-02':*days);
                daynbr = %rem(weeks:7);
                if daynbr = 0;
                   xx_begin = xx_end - %days(9);
                   xx_end   = xx_begin + %days(6);
                endif;
             endif;

             if ta_flag2 = 5;
       //  find thursday
                weeks = %diff(xx_end:d'1901-01-03':*days);
                daynbr = %rem(weeks:7);
                if daynbr = 0;
                   xx_begin = xx_end - %days(10);
                   xx_end   = xx_begin + %days(6);
                endif;
             endif;

             if ta_flag2 = 6;
       //  find friday
                weeks = %diff(xx_end:d'1901-01-04':*days);
                daynbr = %rem(weeks:7);
                if daynbr = 0;
                   xx_begin = xx_end - %days(11);
                   xx_end   = xx_begin + %days(6);
                endif;
              endif;

              if ta_flag2 = 7;
       //  find saturday
                weeks = %diff(xx_end:d'1901-01-05':*days);
                daynbr = %rem(weeks:7);
                if daynbr = 0;
                   xx_begin = xx_end - %days(12);
                   xx_end   = xx_begin + %days(6);
                endif;
              endif;

              if daynbr = 0;
                 callp K3S_3435CL(comp:
                                  buyr:
                                  regn:
                                  locn:
                                  supl:
                                  suplsub:
                                  suplgp1:
                                  suplgp2:
                                  suplgp3:
                                  suplgp4:
                                  suplgp5:
                                  prodgp1:
                                  prodgp2:
                                  prodgp3:
                                  prodgp4:
                                  prodgp5:
                                  save_prt:
                                  save_cpy:
                                  save_ufro:
                                  save_uman:
                                  save_upro:
                                  save_uwat:
                                  save_sdis:
                                  save_slum:
                                  save_snew:
                                  save_sreg:
                                  save_sslo:
                                  save_ublk:
                                  sequence:
                                  xx_begin:
                                  xx_end);
              endif;

          endif;

       endif;

       endsr;
      /end-free
