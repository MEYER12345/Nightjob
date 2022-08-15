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
      **   Name: K3S_2008
      **   Type: ILE RPG Program
      **   Desc: Automatic add of supplier via K_INTSUPL
      **
      *****************************************************************
      **
      **  This program is used to automatically add suppliers.  Suppliers are
      **  added to K_INTSUPL and then to K_SUPLIER if all information is correct and
      **  supplier does not already exist.
      **
      *****************************************************************
      * ------------------------------------ File information data structure
     d/copy k3s_c010

      * -------------------------------------- Program Status Data Structure
     d/copy k3s_c040

     d s0_birth        s               d   datfmt(*iso)
     d s0_birthtm      s               t   timfmt(*iso)
     d s0_chgtype      s              1
     d s0_user         s             10
     d s0_workstn      s             10
     d s0_program      s             10

      *   one time switch
     d #once           s              1
      * ------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------
     d time_stamp      s               z
     d time            s               t
     d date            s               d
      * --------------------------------------------------
     d intsupl_rec   e ds                  ExtName(k_intsupl)
     d buyrgrp_rec   e ds                  ExtName(k_buyrgrp)
     d locatns_rec   e ds                  ExtName(k_locatns)
     d suplier_rec   e ds                  ExtName(k_suplier)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- parameter passed
     d K3S_2008        PI
     d  comp                          1
      *---------------------------------------------------------------------
      *        one time routine
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       if #once = *blanks;
          #once = 'y';

          clear sp_regn;
          clear sp_suplsub;
       endif;

       //------------------------------------------------------------------
       exsr dcliscursor;
       exsr opniscursor;

       dow SQLState = SQLStateOk;

       //fetch next k_intsupl row
         exec sql
          fetch next
           from iscursor
           into :intsupl_rec;

         if SQLState = RowNotFound;
            leave;
         endif;

       //don't allow buyer 0 or blank into system

         exec sql
           select *
              into :buyrgrp_rec
              from k_buyrgrp
              where by_comp = :is_comp and
                    by_buyr = :is_buyr
              fetch first row only;

          if SQLState = SQLStateOk;

             exec sql
              select *
                 into :locatns_rec
                 from k_locatns
                 where lc_comp = :is_comp and
                       lc_locn = :is_locn
                 fetch first row only;

             if SQLState = SQLStateOk;

                exec sql
                 select *
                    into :suplier_rec
                    from k_suplier
                    where sp_comp = :is_comp and
                          sp_locn = :is_locn and
                          sp_supl = :is_supl and
                          sp_suplsub = :is_suplsub
                    fetch first row only;

       //establish new supplier
                if SQLState = RowNotFound;

                   sp_locn    = lc_locn;
                   sp_headcst = lc_headcst;
                   sp_linecst = lc_linecst;
                   sp_service = lc_service;
                   sp_regn    = lc_regn;
                   sp_brkothr = lc_brkothr;
                   sp_convpkp = lc_convpkp;
                   sp_dsptot1 = lc_dsptot1;
                   sp_dsptot2 = lc_dsptot2;

                   if is_name = *blanks;
                      is_name = '** Supplier missing name';
                   endif;

                   if is_leadtm < 1;
                      is_leadtm = 1;
                   endif;

                   if is_orcycle < 1;
                      is_orcycle = 1;
                   endif;

                   sp_comp =     is_comp;
                   sp_locn =     is_locn;
                   sp_buyr =     is_buyr;
                   sp_supl =     is_supl;
                   sp_suplsub =  is_suplsub;
                   sp_name    =  is_name;
                   sp_orcycle =  is_orcycle;
                   clear sp_altsrce;
                   if sp_suplsub = 'h';
                      sp_altsrce = 2;
                   endif;
                   clear sp_group1;
                   clear sp_group2;
                   clear sp_group3;
                   clear sp_group4;
                   clear sp_group5;
                   clear sp_ordate;
                   clear sp_rvdate;
                   sp_dispseq = is_supl;
                   clear sp_fxcfrq;
                   clear sp_fxcday;
                   clear sp_fxclst;
                   clear sp_fxcnxt;
                   sp_leadtmq = is_leadtm;
                   sp_leadtmo = is_leadtm;
                   sp_leadtmv = 20.0;
                   clear sp_leadtms;
                   clear sp_leadtmt;
                   clear sp_svceprv;
                   clear sp_soqseq#;
                   clear sp_rnkbuy$;
                   clear sp_rnkbuyu;
                   clear sp_rnkloc$;
                   clear sp_rnklocu;
                   clear sp_rnkcom$;
                   clear sp_rnkcomu;
                   clear sp_rnkdate;
                   sp_maxdays = lc_fbmax;
                   clear sp_precise;
                   clear sp_autopo;
                   clear sp_potype;
                   clear sp_rebate;
                   sp_procalt = 1;
                   clear sp_cashdsc;
                   sp_prefseq = 999;
                   sp_discbkt = 0;
                   clear sp_actprod;
                   clear sp_recalc;

                   callp K3S_Retrieve_Timestamp(time_stamp);
                   date = %date(time_stamp);
                   sp_birth = date;
                   sp_lastupd = date;

                   exsr $_insert_suplier;

                   exsr $_add_2707;

                   exec sql
                      delete from k_intsupl
                         where current of iscursor;

                endif;
             endif;
          endif;
       enddo;

       exsr clsiscursor;

       *inlr = *ON;

       ///////////////////////////////////////////////// Add supplier

       begsr $_add_2707;

       callp K3S_Retrieve_Timestamp(time_stamp);
       date = %date(time_stamp);
       time = %time(time_stamp);
       s0_birth = date;
       s0_birthtm = time;
       s0_chgtype = 'A';
       s0_user = psds_user;
       s0_program = psds_progm;

       callp K3S_2707(sp_comp:
                      sp_locn:
                      sp_supl:
                      sp_suplsub:
                      s0_birth:
                      s0_birthtm:
                      s0_chgtype:
                      s0_user:
                      s0_workstn:
                      s0_program:
                      sp_buyr:
                      sp_regn:
                      sp_name:
                      sp_dispseq:
                      sp_group1:
                      sp_group2:
                      sp_group3:
                      sp_group4:
                      sp_group5:
                      sp_fxcfrq:
                      sp_fxcday:
                      sp_fxcnxt:
                      sp_headcst:
                      sp_linecst:
                      sp_leadtmq:
                      sp_leadtmo:
                      sp_leadtmv:
                      sp_orcycle:
                      sp_service:
                      sp_maxdays:
                      sp_add_day:
                      sp_precise:
                      sp_autopo:
                      sp_potype:
                      sp_discbkt:
                      sp_brkothr:
                      sp_convpkp:
                      sp_dsptot1:
                      sp_dsptot2:
                      sp_procalt:
                      sp_rebate:
                      sp_cashdsc:
                      sp_prefseq:
                      sp_reqsplt:
                      sp_spltype:
                      sp_exclead:
                      sp_cmbtype);

       endsr;

       begsr dcliscursor;
       exec sql
       Declare iscursor cursor
         for
         select *
         from k_intsupl
         where is_comp = :comp
         order by is_comp,
                  is_locn,
                  is_supl,
                  is_suplsub;
       endsr;

       begsr opniscursor;
         exec sql
           open iscursor;
       endsr;

       begsr clsiscursor;
         exec sql
           close iscursor;
       endsr;

       begsr $_insert_suplier;
         exec sql
            insert into k_suplier
               values (:suplier_rec);
       endsr;

      * ////////////////////////////////////////////////////////////////////
