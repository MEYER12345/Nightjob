      *****************************************************************
     h copyright('(C) Copyright 1996 - 2004 King III Solutions, Inc.  +
     h Rel 4.33 2004-01-02       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)
     h OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2004 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_1053
      **   Type: ILE RPG Program
      **   Desc: Check supplier for automatic PO approval NIGHT Job
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/26/2014.
      **  Remarks. Altered program to utilize an SQL cursor to loop
      **           through K_SUPLSOQ file records and perform the
      **           record selections formerly done by OPNQRYF                                      .
      **           statements in K3S_1053CL.
      *****************************************************************
     d buyr            s                   like(so_buyr)
     d locn            s                   like(so_locn)
     d supl            s                   like(so_supl)
     d suplsub         s                   like(so_suplsub)
     d soqseq#         s                   like(so_soqseq#)
     d po#             s                   like(so_po#)
     d mode            s              8a   inz('  view  ')
     d @returned       s              3  0                                      how user returned
     d @updated        s              1  0                                      record updated flag

     d time_stamp      s               z   inz                                  batch number

      * ---------------------------------------------------- Local Data Area
     d*copy k3s_c030
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-1999 by King III Solutions, Inc.
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
     d lda_slview                     1p 0                                      order summary view
     d lda_cnv                       10                                         order summary view
     d lda_dly                       10                                         order summary view
     d lda_dta                       10                                         order summary view
     d lda_mod                       10                                         order summary view
     d lda_obj                       10                                         order summary view
     d lda_qry                       10                                         order summary view
      * parameters passed to program
     d cmsysdate       s               d
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- parameter passed
     d K3S_1053        PI
     d  comp                          1
      * ------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;

       exec sql
         select cm_sysdate
           into :cmsysdate
           from k_company
           where cm_comp = :comp
           fetch first row only;

       //set up LDA
       in        *dtaara;
       callp K3S_Retrieve_Timestamp(time_stamp);
       lda_comp = comp;
       lda_cmpdat = cmsysdate;
       lda_sysdat = %date(%subst(%char(time_stamp):1:10):*ISO);
       lda_usradj = 0;
       lda_alarm  = 0;
       lda_dealex = 0;
       lda_date6  = 0;
       lda_managr = 0;
       lda_window = 0;
       lda_orview = 0;
       lda_slview = 0;
       lda_prview = 0;
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
       out       *dtaara;

       //Process the suplsoq file
       exsr $_prc_supl;

       //finished, set on LR
       *inlr = *on;

       /////////////////////////////////////////// Process the prodsoq file

       begsr $_prc_supl;

       exsr dclsocursor;
       //exsr clssocursor;
       exsr opnsocursor;

       dow SQLState = SQLStateOk;

       //get supplier suggested order
           exec sql
            fetch next
             from socursor
             into :suplsoq_rec;

           if SQLState = RowNotFound;
              leave;
           endif;

       // check suplsoq fields
           if so_soqtype = 'FC' AND    //Fixed cycle
              so_autopo  = 1    AND
              so_actunet > 0    AND
              so_altsrce = 0    AND    //Regular source supplier
              so_po#     = *blanks;
                 comp = so_comp;
                 locn = so_locn;

                 buyr = so_buyr;
                 supl = so_supl;
                 suplsub = so_suplsub;
                 soqseq# = so_soqseq#;
                 po# = *blanks;
                 mode = '  view  ';
                 @updated = 0;
                 @returned = 0;
                 callp K3S_1050AU(comp:
                                  locn:
                                  buyr:
                                  supl:
                                  suplsub:
                                  soqseq#:
                                  po#:
                                  mode:
                                  @updated:
                                  @returned);
           endif;
       enddo;
       exsr clssocursor;
       endsr;

       begsr dclsocursor;
       exec sql
        declare socursor Cursor
         for
         select *
         from k_suplsoq
         where so_comp = :comp
         order by so_comp,
                  so_buyr,
                  so_locn,
                  so_supl,
                  so_suplsub,
                  so_soqseq#;
       endsr;

       begsr opnsocursor;
       exec sql
        open socursor;
        if SQLState <> SQLStateOk;
           exsr clssocursor;
           exec sql
            open socursor;
        endif;
       endsr;

       begsr clssocursor;
       exec sql
        close socursor;
       endsr;
      /end-free

