      *****************************************************************
     h copyright('(C) Copyright 1996 - 2018 King III Solutions, Inc.  +
     h Rel 5.5  2018-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h OPTION(*NODEBUGIO:*SRCSTMT)
     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5')
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2018 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_1500
      **   Type: ILE RPG Program
      **   Desc: Create Supplier Suggested Orders
      **
      *****************************************************************
      **
      **  This program reads products from file k_product that were
      **  assembled using OPNQRYF. This OPNQRYF set should only contain
      **  data for one company at a time. Multiple locations can be
      **  processed during the same batch. For performance, make sure
      **  sequence is in company, location, buy group, supplier,
      **  sub supplier. A parameter will be passed to
      **  this program to determine if this is a day time or nite time
      **  run of the program. The day time session will normally build
      **  an order for one supplier, while the nite time session will
      **  build orders for all suppliers at the specified company
      **  and location.
      **
      *****************************************************************
      **
      **  Indicator usage
      **
      **      record formats
      **  11  rk_suplier
      **  12  rk_suplsoq
      **  13  rk_locatns
      **  14  rk_supldis
      **  15  rk_product
      **  16  rk_buyrsrv
      **
      *****************************************************************
      **
      **  this program does contain a few enhancements that were done
      **  specifically for Cardinal Health/Bindley Western division.
      **  they have been identified with <<CH/BW>> so that we can
      **  remove them, or use them in Release 5.0 with changes.
      **
      *****************************************************************
      **
      **  This program does contain a section of logic that was given
      **  to us from Core-Mark. This logic deals with slower movers
      **  where the amount per order cycle is between .5 and 1.5
      **  cartons. This is a TRACS modification, and the logic is
      **  controlled by a table code record type of QC1, and then the
      **  different chains (Buy Groups) can be specified to use this
      **  logic. This logic is identified with <<CORE-MARK>>.
      **
      *****************************************************************
      **
      **  08/10/2017 King3 - Important notice for the future.
      **                     Usually this program would have been
      **                     expected to populate PQ_NONSTCK from PR_NONSTCK
      **                     as it does for most fields in K_PRODUCT.
      **                     However, since that is not the case, then I stole
      **                     the use of the field for an application at
      **                     Les Schwab Tires back in 6/4/2013, but I did
      **                     wrap it with an APP record 'K3S_1500  SHUT_OFF'
      **                     where only LST should have flag1 = 1.
      **                     Franklin Machine Products is very much using
      **                     PR_NONSTCK = 1 for Non-stocks. In a future
      **                     database release, adding a separate field
      **                     for the LST shut off would be good.
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 07/9/2014.
      **  Remarks. Converted this program to free format RPG.
      *****************************************************************

      * ---------------------------------- local data area *LDA definitions
     d/copy k3s_c035

      * -------------------------------------- Program Status Data Structure
     d/copy k3s_c040

      * ---------------------------------- calculate safety stock components
     d*copy k3s_c090

      * ---------------------------------- calculate usage (days into units)
     d*copy k3s_c100

      * ----------------------------------------- calculations with GK table
     d/copy k3s_c110

      * --------------------------------- calculate suggested order quantity
     d*copy k3s_c120

      * ------------------------------------ calculate manual min/max values
     d*copy k3s_c130

      * ------------------------------------- Used to calculate delay values
      *   expected stock out for a time value
     d exptout         s             15  3
      *   expected stock out array for products
     d expstkout       s             15  3 dim(6)
      *   expected stock out array for supplier order
     d expstkoutt      s             15  3 dim(6)
      *   specified stock out for product
     d specout         s             15  3
      *   specified stock out for supplier order
     d specoutt        s             15  3

      * ----------------------------------------- Supplier for Control Break
     d
     d supl            ds
     d  #prlocn                       5
     d  #prsupl                      10
     d  #prsuplsub                   10
     d suplsaved       s                   like(supl)

      * ------------------------------------------- Buy group/location break
     d                 ds
     d buyrlocn                      10
     d  prbuyr                        5    overlay(buyrlocn:1)
     d  prlocn                        5    overlay(buyrlocn:6)
     d buyrlocnsv      s                   like(buyrlocn)

      * ------------------------------ Save reg supl order cycle, alt source
     d savorcycle      s                   like(so_orcycle)

      * -------------------------------------- Save reg product buy multiple
     d savbuymult      s                   like(pr_buymult)

      * -------------------------------------- Hold out quantity work fields
     d hold_locn       s                   like(pr_cmblocn)
     d hold_supl       s                   like(pr_cmbsupl)
     d hold_sub        s                   like(pr_cmbsub)

      *** <<CH/BW>>
      * ------------------------------------- automated add days work fields
     d add_rectyp      s                   like(aa_rectype)
     d add_locn        s                   like(pr_cmblocn)
     d add_supl        s                   like(pr_cmbsupl)
     d add_sub         s                   like(pr_cmbsub)
     d add_prod        s                   like(pr_prod)

      * ------------------------------------------------ Day time flag (1,0)
      *   Night run = 0
      *   Day time  = 1
      *day_time        s              1  0
      *
      * ---------------------------------------------- Alt source flag (1,0)
      *   Regular order = 0
      *   Alternate source order = 1
      *alt_sour        s              1  0
      *
      * ---------------------------------- Force safety stock re-calculation
      *   Do not force ss recalc    = 0
      *   Re-calculate safety stock = 1
      *re_calc_ss      s              1  0
      *
      * ------------------------- Re-calculate order for supplier constraint
      *   Do not re-calculate order = 0
      *   Re-calculate order        = 1
      *supl_const      s              1  0
      *
      * ---------------------------------------------------- Use deal system
      *   Do not use deal system    = 0
      *   Use deal system
      *use_deals       s              1a
      *
      *   SOQ sequence #            = 5
     d soqseq#sav      s                   like(pq_soqseq#)
     d came_from       s                   like(pq_soqseq#)

      * --------------------------------------------------------- Workfields
     d #once           s              1  0                                      once routine
     d #notfirst       s              1                                         don't write supplier
     d #do_once        s              1  0                                       calc # of days once
     d first12         s               d
     d first13         s               d
     d first52         s               d
     d today           s               d
      *
     d time            s              9  3
     d backout         s              1
     d foundit         s              1
     d tx              s             11  3
     d cx              s             15  3
     d bx              s             15  3
     d timesave        s              9  3
     d kkk             s              2  1
     d z               s              1  0
     d i               s              1  0
     d minday          s              9  3
     d maxday          s              9  3
     d minunit         s              7  0
     d maxunit         s              7  0
     d mnwork          s              7  0
     d mxwork          s              7  0
     d delay           s              1  0
     d dealtest        s              1
     d extension       s             15  4
     d first_day       s               d   inz(d'0001-01-01') datfmt(*iso)
     d time_stamp      s               z   inz
     d part_cycle      s              9  2
     d diff_days       s              7  0
     d allocate        s              1
     d testvalue       s             15  4
     d fbuy_check      s              1  0
     d supl_name       s              5
     d savsoqseq#      s                   like(pq_soqseq#)
     d soqovrdsav      s                   like(pq_soqovrd)
     d soqactw1        s                   like(pq_soqact)
     d soqactw2        s                   like(pq_soqact)
     d soqactsv        s                   like(pq_soqact)
     d soqactkeep      s                   like(pq_soqact)
     d safesum         s                   like(pr_safesum)
     d convpak         s                   like(pr_convpak)
     d convpak1        s                   like(pr_convpak)
     d convpak2        s                   like(pr_convpak)
     d convpak3        s                   like(pr_convpak)
     d dealwindow      s                   like(pr_dealbeg)
     d locn            s              5
     d program         s             10
     d extra_days      s              5  2                                      value dating %
     d least_1         s              1
     d check_pr        s              1  0                                      probation check type
     d check_u         s              1  0                                      U1-U4 exit
     d check_RE        s              1  0                                      RE Check processing
     d typnote_RE      s              1                                         type note-reminder
     d typval_nt       s              1
     d locn_nt         s                   like(nt_locn)
     d prod_nt         s                   like(nt_prod)
     d supl_nt         s                   like(nt_supl)
     d suplsub_nt      s                   like(nt_suplsub)
     d deal_nt         s                   like(nt_deal)
     d seasonl_nt      s                   like(nt_seasonl)
     d total_out       s              1                                         total out of stock
     d excl_buy        s              1  0                                      Exclude buying logic
     d rx_otc          s              1  0                                      Rx or OTC, Bindley
     d vary_orcyc      s              1  0                                      Variable Order Cycle
     d save_orcyc      s                   like(sp_orcycle)                     Variable Order Cycle
     D Weeks           S              7  0
     D DayNbr          S              1P 0
     d max_to_min      s              1  0                                      Force Max to Min flg
     d user_group      s              1  0                                      Group orders by User
     d rm_prv_ord      s              1  0                                      Remove prev orders
     d altsrc_all      s              1  0                                      AltSrc all products
     d perm_deal       s              1  0                                      Permanent deals
     d average_0       s              1  0                                      Average 0 allowed
     d must_recv       s              1  0                                      Must receive 1st
     d not_recvd       s               d   inz(d'0001-01-01') datfmt(*iso)
     d all_cost_0      s              5  0                                      All within supl $0
     d all_minmax      s              5  0                                      All within supl mnmx
     d all_days        s              5  0                                      days to advance next
     d shut_off        s              1  0                                      shut off SOQ
     d days_out        s              3  0                                      days to look out
     d date_out        s                   like(pr_birth)                       date to look out
     d date_outA       s             10                                         date to look out
     d date_outX       s             10                                         date to look out
     d WrkNextPer      s                   like(pr_forcper)                     next period #
     d all_frozen      s              1  0                                      all frozen switch
     d all_froz_0      s              7  0                                      all frozen counter
     d pit_perAlp      s              3
     d pit_percnt      s              2  2
     d pit_whole       s              3  0
     d mxtest          s              7
     d pit_max         s              2  0
     d Pitco_Log       s              1  0
     d Pitco_SScap     s              3  0
     d Pitco_typ       s              1  0
     d pit_min         s              2  0
     d y               s              3  0
     d j               s              3  0
     d u1sttest        s             10
     d taflag1         s              1  0
     d taflag2         s              1  0
     d tanumber1       s              5  0
     d tanumber2       s              7  2
     d tanumber3       s             11  4
     d tacodeds1       s            100
     d CoreMarkCount   s              7  0
     d bsminsvc        s              3  1
     d g1minsvc        s              3  1
     d prodhld_count   s              5  0
     d L1_count        s              5  0
     d P1_count        s              5  0
     d P2_count        s              5  0
     d S1_count        s              5  0
     d S2_count        s              5  0
     d notepad_count   s              5  0
     d suplsoq_count   s              5  0
     d #forcint        s              3  0
     d default_time    s               t   timfmt(*hms)
     d                                     inz(t'00.00.00')

     d pe_warning      s              1                                         pe_warning system
     d ending_52       s                   like(se_ending)                      ending date 52
     d ending_12       s                   like(se_ending)                      ending date 12
     d ending_13       s                   like(se_ending)                      ending date 13
     d ending_dt       s                   like(se_ending)                      ending date work
     d ending_SET      s                   like(se_ending)                      setll ending work
     d reqtype_pd      s                   like(pd_reqtype)                     request type
     d xx_locn         s                   like(pr_locn)                        location
     d xx_supl         s                   like(pr_supl)                        supplier
     d xx_suplsub      s                   like(pr_suplsub)                     sub-supplier
     d*
     d pqsoqact        s              7  0
     d pqsoqovrd       s              1  0
     d prodsez_found   s               n
     d #prep           s              1
     d*
     d sddiscbkt       s              1  0
     d sddis1typ       s              1  0
     d sddis1val       s              7  0
     d sddis1unt       s              1  0
     d sddis2typ       s              1  0
     d sddis2val       s              7  0
     d sddis2unt       s              1  0
     d sddisrate       s              3  1
     d sddissavg       s              5  0
     d #sddiscbkt      s              1  0


      *** <<CH/BW>>
     d L1_add_day      s              1                                         automated add days
     d P1_add_day      s              1                                         automated add days
     d P2_add_day      s              1                                         automated add days
     d S1_add_day      s              1                                         automated add days
     d S2_add_day      s              1                                         automated add days
     d test_STOP       s              4                                         automated add days

      *** <<CORE-MARK>>
     d qc1_on          s              1  0                                      Core-Mark logic?
     d qc1_chain       s              1  0                                      Use this chain?
     d qc1_expect      s              9  2                                      expected demand
     d qc1_exprnd      s              7  0                                      exp demand rounded


      *** car count logic work fields
     d CarLogic        s              1  0                                      car logic used?
     d CarCount        s              7  0                                      number cars
     d CarRemain       s              7  0                                      remainder
     d G1_Logic        s              1  0                                      G1 logic used?
     d wrkGroup2       s                   like(pr_group2)                      for G1 logic

     d DELAY_FC        s              1  0                                      delay Fixed Cycle
     d DELAY_VOC       s              1  0                                      delay Var Ord Cycle
     d NOT_TODAY       s              1  0                                      is this order day?

     d OCA_recent      s             10                                         get last OCA date
     d recent_ISO      s               d   datfmt(*ISO)                         recent OCA in *ISO
     d OCA_days        s              7  0                                      value dating %

     d ManMinMaxR      s              1  0                                      manual min/max rev

      * ------------------------------ work fields passed to module K3S_M070
     d vldatng         s              3  3                                      value dating %
     d mode#           s              1  0                                      Mode=1 from K3S_1500
     d disc_accum      s             11  4                                      accum disc eaches
     d off_invce       s             11  4                                      Off invoice
     d off_reg         s             11  4                                      Off invoice
      * ------------------------------ work fields passed to module K3S_M071
     d cashdsc         s              3  3                                      cash discount
     d rebate          s              3  3                                      rebate
     d intrate         s              3  3                                      annual interest
     d return          s              3  3                                      return on investment
     d invmeth         s              1  0                                      investment method
     d deal_days       s              3  0                                      extra days to buy
     d restr_qty       s              7  0                                      restricted quantity
     d last_recd       s              1  0                                      last record flag

     d @s_repcary      s              3  3
     d @s_linecst      s              5  2
     d @s_orcycle      s              3  0
     d @s_forcint      s              3  0
     d @s_buymult      s              7  0
     d @s_minqty       s              7  0
     d @s_costeac      s             11  4
     d @s_forcast      s              9  2
     d @s_fordevp      s              3  3
     d @s_service      s              3  3
     d @s_leadtm       s              3  0
     d @s_leadtmv      s              3  3
     d @s_formeth      s              1  0
     d @s_sstimef      s              9  3
     d @s_otimfac      s              3  0
     d @s_devtime      s              7  3
     d @s_intrval      s              5  0
     d meandevary      s              5  4 dim(71)                              mean dev runtime ary
     d strddevary      s              5  4 dim(37)                              std dev runtime ary

     d @q_maxunit      s              7  0
     d @q_minunit      s              7  0
     d @q_qtybaln      s              7  0
     d @q_minqty       s              7  0
     d @q_buymult      s              7  0
     d @q_convpak      s              7  0
     d @q_convpkp      s              3  1
     d @q_soq          s              7  0

     d  @m_altsour     s              1  0
     d  @m_maminid     s              3  0
     d  @m_mamaxid     s              3  0
     d  @m_maminiu     s              7  0
     d  @m_mamaxiu     s              7  0
     d  @m_qtybaln     s              7  0
     d  @m_chkopnt     s              1  0
     d  @m_sochk       s              5  0
     d  @m_seasonl     s             10
     d  @m_forcast     s              9  2
     d  @m_forcper     s              3  0
     d  @m_forcint     s              3  0
     d  @m_longtrm     s              3  2
     d  @m_minday      s              9  3
     d  @m_maxday      s              9  3
     d  @m_minunit     s              7  0
     d  @m_maxunit     s              7  0
     d  @m_mnwork      s              7  0
     d  @m_mxwork      s              7  0
     d  @m_str_day     s              1  0
     d  @m_str_unt     s              1  0

     d  @u_comp        s              1
     d  @u_seasonl     s             10
     d  @u_days        s             11  3
     d  @u_forcast     s              9  2
     d  @u_forcper     s              3  0
     d  @u_forcint     s              3  0
     d  @u_longtrm     s              3  2
     d  @u_dowmap      s              1  0
     d  @u_dowper1     s              5  2
     d  @u_dowper2     s              5  2
     d  @u_dowper3     s              5  2
     d  @u_dowper4     s              5  2
     d  @u_dowper5     s              5  2
     d  @u_dowper6     s              5  2
     d  @u_dowper7     s              5  2
     d  @u_sstper1     s              5  2
     d  @u_sstper2     s              5  2
     d  @u_sstper3     s              5  2
     d  @u_sstper4     s              5  2
     d  @u_sstper5     s              5  2
     d  @u_sstper6     s              5  2
     d  @u_sstper7     s              5  2
     d  @u_diff12      s              3  0
     d  @u_diff13      s              3  0
     d  @u_diff52      s              3  0
     d  @u_use         s             15  3
     d  @u_Ds_flag     s              1  0                                      Calculate results?
     d  @u_DayNbr      s              1  0                                      Today's day #
     d  @u_Ds_days     s              2  0                                      Display days
     d  @u_Ds_wdis     s              5  2 dim(12)                              Weekly Dist %
     d  @u_Ds_dlyu     s             15  4 dim(12)                              Daily usage
     d  @u_Ds_spct     s              5  2 dim(12)                              Safety Stock %
     d  @u_Ds_susg     s             15  4 dim(12)                              Safety Stock usage
     d  @u_Ds_accu     s             15  4 dim(12)                              Accumulated usage
     d  @u_Ds_prno     s              3  0 dim(12)                              Profile factor
     d  @u_Ds_fctr     s              5  2 dim(12)                              Profile factor
     d  @u_Ds_warn     s              1  0


      * work fields used in subprocedure

      *   profile array
     d
     d @u_ixa          ds
     d  pzfactr01                     5s 2
     d  pzfactr02                     5s 2
     d  pzfactr03                     5s 2
     d  pzfactr04                     5s 2
     d  pzfactr05                     5s 2
     d  pzfactr06                     5s 2
     d  pzfactr07                     5s 2
     d  pzfactr08                     5s 2
     d  pzfactr09                     5s 2
     d  pzfactr10                     5s 2
     d  pzfactr11                     5s 2
     d  pzfactr12                     5s 2
     d  pzfactr13                     5s 2
     d  pzfactr14                     5s 2
     d  pzfactr15                     5s 2
     d  pzfactr16                     5s 2
     d  pzfactr17                     5s 2
     d  pzfactr18                     5s 2
     d  pzfactr19                     5s 2
     d  pzfactr20                     5s 2
     d  pzfactr21                     5s 2
     d  pzfactr22                     5s 2
     d  pzfactr23                     5s 2
     d  pzfactr24                     5s 2
     d  pzfactr25                     5s 2
     d  pzfactr26                     5s 2
     d  pzfactr27                     5s 2
     d  pzfactr28                     5s 2
     d  pzfactr29                     5s 2
     d  pzfactr30                     5s 2
     d  pzfactr31                     5s 2
     d  pzfactr32                     5s 2
     d  pzfactr33                     5s 2
     d  pzfactr34                     5s 2
     d  pzfactr35                     5s 2
     d  pzfactr36                     5s 2
     d  pzfactr37                     5s 2
     d  pzfactr38                     5s 2
     d  pzfactr39                     5s 2
     d  pzfactr40                     5s 2
     d  pzfactr41                     5s 2
     d  pzfactr42                     5s 2
     d  pzfactr43                     5s 2
     d  pzfactr44                     5s 2
     d  pzfactr45                     5s 2
     d  pzfactr46                     5s 2
     d  pzfactr47                     5s 2
     d  pzfactr48                     5s 2
     d  pzfactr49                     5s 2
     d  pzfactr50                     5s 2
     d  pzfactr51                     5s 2
     d  pzfactr52                     5s 2
     d @u_ix                          5s 2 dim(52)
     d                                     overlay(@u_ixa)

      * -------------------------------------------------------
     d product_rec   e ds                  ExtName(k_product)
     d locatns_rec   e ds                  ExtName(k_locatns)
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
     d suplier_rec   e ds                  ExtName(k_suplier)
     d supldis_rec   e ds                  ExtName(k_supldis)
     d suplvoc_rec   e ds                  ExtName(k_suplvoc)
     d prodhld_rec   e ds                  ExtName(k_prodhld)
     d prodwkd_rec   e ds                  ExtName(k_prodwkd)
     d dealper_rec   e ds                  ExtName(k_dealper)
     d logprod_rec   e ds                  ExtName(k_logprod)
     d autoadd_rec   e ds                  ExtName(k_autoadd)
     d prodsed_rec   e ds                  ExtName(k_prodsed)
     d schedpe_rec   e ds                  ExtName(k_schedpe)
     d notepad_rec   e ds                  ExtName(k_notepad)
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
      * -------------------------------------------------------
     d StmtString      s           1024a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
     d SelcritCL       s            300a   inz
     d SelcritSP       s            300a   inz
     d SelcritAP       s            300a   inz
     d Sortseq         s            300a   inz
      * -------------------------------------------------------
     d #cur1val        s                   like(so_cur1val)
     d #cur1unt        s                   like(so_cur1unt)
     d #tstcur1val     s                   like(so_cur1val)
     d #tstcur1unt     s                   like(so_cur1unt)
     d discbkt_ind     s               n   inz(*off)
     d #comp           s                   like(so_comp)
     d #buyr           s                   like(so_buyr)
     d #locn           s                   like(so_locn)
     d #supl           s                   like(so_supl)
     d #suplsub        s                   like(so_suplsub)
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_1500        PI
     d  comp                          1
     d  day_time                      1  0                                      Nite run = 0,Day = 1
     d  alt_sour                      1  0                                      Reg.= 0,Alternate=1
     d  re_calc_ss                    1  0                                      Recalc ss = 1
     d  supl_const                    1  0                                      Not recalc=0,recal=1
     d  use_deals                     1                                         Not use deals=0
     d  soqseq#                       5  0
     d  product                      25
     d  keepqty                       1
     d  pgm                          10
     d  buyr                          5
     d  location                      5
     d  suplier                      10
     d  suplsub                      10
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;
       //----------------------------------------------------- Read Products

       //start with first product in OPNQRYF

       soqseq#sav = soqseq#;

       //for day time processing
       //   when from K3S_1010 'came_from' = *zeros
       //   when from K3S_1020 'came_from' > *zeros
       came_from  = soqseq#;

       exsr IntSQLStmtpr;

       exsr dclprcursor;
       //exsr clsprcursor;
       //exsr clssocursor;
       exsr dclaacursor1;
       exsr dclaacursor2;
       exsr dclpqcursor;
       exsr dclpucursor;
       exsr dclpdcursor;
       exsr dclsecursor;

       // ---------------------------------------------------------- Main Loop
       // main loop

        #prep = 'N';
        exsr PrepDynSQLStmtpr;
        if SQLState = SQLStateOk;

           #prep = 'Y';
    ‚   //open dynamic cursor
           exsr opnprcursor;

       dow SQLState = SQLStateOk;

           exec sql
             fetch next
               from prcursor
               into :product_rec;

           if SQLState = RowNotFound;
              leave;
           endif;


       //day time check keep quantity
         if (day_time = 1) and
            (keepqty = '1');

       // regular suppliers
           if pr_altsrce = 0;
              exec sql
                select pq_soqact, pq_soqovrd
                  into :pqsoqact, :pqsoqovrd
                  from k_prodsoq
                  where pq_comp = :pr_comp and
                        pq_buyr = :pr_buyr and
                        pq_locn = :pr_locn and
                        pq_supl = :pr_supl and
                        pq_suplsub = :pr_suplsub and
                        pq_soqseq# = :soqseq#sav and
                        pq_prod = :pr_prod
                  fetch first row only;
              if SQLState = SQLStateOk and
                 pqsoqovrd = 1;
                   soqactkeep = pqsoqact;
                   soqovrdsav = pqsoqovrd;
              else;
                   soqactkeep = 0;
                   soqovrdsav = 0;
              endif;
           endif;

       // combined suppliers
           if pr_altsrce = 2;

              soqactkeep = 0;
              soqovrdsav = 0;

              exsr InzInpSrchpq;
    ‚   //initialize StmtString
              exsr IntSQLStmtpq;

    ‚          exsr PrepDynSQLStmtpq;

              if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                 exsr opnpqcursor;

                 dow SQLState = SQLStateOk;

                     exec sql
                       fetch next
                          from pqcursor
                          into :prodsoq_rec;

                          if SQLState = RowNotFound;
                             leave;
                          endif;

                          if SQLState = SQLStateOk and
                             pq_cmblocn = pr_cmblocn and
                             pq_cmbsupl = pr_cmbsupl and
                             pq_cmbsub = pr_cmbsub and
                             pq_soqovrd = 1;
                                soqactkeep = pq_soqact;
                                soqovrdsav = pq_soqovrd;
                          endif;
                 enddo;
                 exsr clspqcursor;
              endif;
           endif;
         endif;


       //------------------------------------------------------ Once Routine

       //once routine
         if #once <> 1;
            #once = 1;

            exsr $_loadarrays;
        //initialize do once flag for retireving first day of nex period
            #do_once = 0;

       //retriev local data area *lda only if it is night time
            if day_time = 0;
               in *dtaara;
            endif;

       //for alternat source orders, force re-calculation of safety stk
            if alt_sour = 1;
               re_calc_ss = 1;
            endif;

       //prime key lists
       //company

       //  initialize type of note to look for - reminders
            typnote_RE = 'R';

       //sav Probation check method
       //    check_pr   = 1 means that PR checks only occur during the
       //                   last 10 days of the probation period
       //    check_pr   = 0 means that PR checks occur all throughout the
       //                   probation period

             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'CHECK_PR  PROBATION '
                 fetch first row only;
             if SQLState = SQLStateOk;
               check_pr   = taflag1;
             else;
               check_pr   = 0;
             endif;

       //sav U1 - U4 processing exit
       //    check_u    = 1 means exit to U1 - U4 processing by calling
       //                   program K3S_1530 after each order built.
       //    check_u    = 0 means no U1 - U4 processing

             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'CHECK_U   U1_U4_EXIT'
                 fetch first row only;
             if SQLState = SQLStateOk;
               check_u    = taflag1;
             else;
               check_u    = 0;
             endif;

       //Remov Previous Orders (Store Level environment from K3S_1500SP)
       //    rm_prv_ord = 1 means K3S_1500 is running via K3S_1500SP, and
       //                   all previous suggested orders (except 'AP'
       //                   approved) should be deleted
       //    rm_prv_ord = 0 follow normal logic, this is not Store Level
       //                   environment

             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_1500  RM_PRV_ORD'
                 fetch first row only;
             if SQLState = SQLStateOk;
               rm_prv_ord = taflag1;
             else;
              rm_prv_ord = 0;
             endif;

       //Alternat Source - Look at all products for possible ordering
       //    altsrc_all = 1 means that all products will be looked at for
       //                   possible ordering, not just where PQ_SOQACT > 0
       //    altsrc_all = 0 means that only products where PQ_SOQACT > 0
       //                   will be reviewed for possible ordering from
       //                   alternate sources

             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_1500  ALTSRC_ALL'
                 fetch first row only;
             if SQLState = SQLStateOk;
               altsrc_all = taflag1;
             else;
               altsrc_all = 0;
             endif;

       //Sav RE Check processing flag
       //    check_RE   = 1 means do perform RE Check processing
       //    check_RE   = 0 means do not perform RE Check processing

             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'CHECK_RE  RE_PROCESS'
                 fetch first row only;
             if SQLState = SQLStateOk;
               check_RE   = taflag1;
             else;
               check_RE   = 0 ;
             endif;

       //sav Straight Manual Min/Max Days and Units processing flags
       //    @m_str_day = 1 means use straight days (flag 1)
       //    @m_str_day = 0 means do not use straight days (flag 1)
       //    @m_str_unt = 1 means use straight units (flag 2)
       //    @m_str_unt = 0 means do not use straight units (flag 2)

             exec sql
               select ta_flag1, ta_flag2
                 into :taflag1, :taflag2
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'MIN_MAX   STRAIGHT=1'
                 fetch first row only;
             if SQLState = SQLStateOk;
               @m_str_day = taflag1;
               @m_str_unt = taflag2;
             else;
               @m_str_day = 0;
               @m_str_unt = 0;
             endif;


       //calculat Manual Min/Max Days and Units when PR_FORCAST = 0
       //    average_0 = 1 means to process min/max even when PR_FORCAST = 0
       //    average_0 = 0 means do not process min/max  when PR_FORCAST = 0

             average_0 = 0;
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'MIN_MAX   AVERAGE=0 '
                 fetch first row only;
             if SQLState = SQLStateOk and taflag1 = 1;
                average_0 = 1;
             endif;

       //Mus receive product before suggested quantity generated,
       //adde logic to test quantity on order for that product
       //    must_recv = 1 means feature is active, product must be received
       //    must_recv = 0 means this feature is not active

             must_recv = 0;
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_1500  MUST_RECV '
                 fetch first row only;
             if SQLState = SQLStateOk and taflag1 = 1;
                must_recv = 1;
             endif;

       //Shu off SOQ under special condition
       //product has seasonal profile, and we're in a period with Factor 0,
       //and a Manual Minimum Units > 0
       //    shut_off  = 1 means feature is active, test to set SOQ to 0
       //    shut_off  = 0 means this feature is not active

             shut_off  = 0;
             exec sql
               select ta_flag1, ta_number1
                 into :taflag1, :tanumber1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_1500  SHUT_OFF  '
                 fetch first row only;
             if SQLState = SQLStateOk and taflag1 = 1;
               shut_off  = 1;
               days_out  = tanumber1;
               if days_out > 15;
                  days_out = 15;
               endif;
             endif;

       //Al frozen and product forecast 0, one product can make order Due
       //supplier set up with all products 'F' Frozen with PR_FORCAST = 0
       //even 1 product needing an SOQ will force supplier order as Due
       //    all_frozen= 1 means feature is active, perform tests
       //    all_frozen= 0 means this feature is not active

             all_frozen = 0;
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_1500  ALL_FROZEN'
                 fetch first row only;
             if SQLState = SQLStateOk and taflag1 = 1;
                all_frozen= 1;
             endif;

       //PE Warning Sysytem
       //    pe_warning= 0 means feature is active, perform tests
       //    pe_warning= 1 means this feature is not active

             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'PE_WARNINGSYSTEM    '
                 fetch first row only;
             if SQLState = SQLStateOk and taflag1 = 1;
                pe_warning = *off;
             else;
                pe_warning = *on;
             endif;

       //IfPE Warning System is active, get Period End dates
            if pe_warning = *on;
               clear ending_52;
               clear ending_12;
               clear ending_13;
       //   weekly

               exsr InzInpSrchse;
    ‚   //initialize StmtString
               exsr IntSQLStmtse;

    ‚           exsr PrepDynSQLStmtse;

               if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                  #forcint = 52;
                  exsr opnsecursor;

                  if SQLState = SQLStateOk;
                     exec sql
                       fetch next
                         from secursor
                         into :schedpe_rec;

                     if SQLState = SQLStateOk;
                        ending_52 = se_ending;
                     endif;
                  endif;
                  exsr clssecursor;
               endif;
       //   monthly

               exsr InzInpSrchse;
    ‚   //initialize StmtString
               exsr IntSQLStmtse;

    ‚           exsr PrepDynSQLStmtse;

               if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                  #forcint = 12;
                  exsr opnsecursor;

                  if SQLState = SQLStateOk;
                     exec sql
                       fetch next
                         from secursor
                         into :schedpe_rec;

                     if SQLState = SQLStateOk;
                        ending_12 = se_ending;
                     endif;
                  endif;
                  exsr clssecursor;
               endif;
       //   13-week

               exsr InzInpSrchse;
    ‚   //initialize StmtString
               exsr IntSQLStmtse;

    ‚           exsr PrepDynSQLStmtse;

               if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                  #forcint = 13;
                  exsr opnsecursor;

                  if SQLState = SQLStateOk;
                     exec sql
                       fetch next
                         from secursor
                         into :schedpe_rec;

                     if SQLState = SQLStateOk;
                        ending_13 = se_ending;
                     endif;
                  endif;
                  exsr clssecursor;
               endif;

            endif;

       //Exclud Buying Until logic to look at 'Back out' quantity?
       //    excl_buy = 1 means use 'back out' quantity in logic
       //    excl_buy = 0 means do not use 'back out' qty in logic

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'EXCL_BUY  BACK_OUT  '
                fetch first row only;
            if SQLState = SQLStateOk;
               excl_buy = taflag1;
            else;
               excl_buy = 0;
            endif;

       //Permanen Deals 'off invoice' testing for 'P2' type only
       //    perm_deal = 1 means we do want to test for condition
       //    perm_deal = 0 means we do not test for condition

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'K3S_1500  K_DEALPER '
                fetch first row only;
            if SQLState = SQLStateOk;
               perm_deal = taflag1;
            else;
               perm_deal = 0;
            endif;

       //Forc calculated Maximum to calculated Minimum
       //    max_to_min = 1 means do set maximum to minimum
       //    max_to_min = 0 means to use normal EOQ logic

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'K3S_1500  MAX_TO_MIN'
                fetch first row only;
            if SQLState = SQLStateOk;
               max_to_min = taflag1;
            else;
               max_to_min = 0;
            endif;

       //Grou Suggested Orders by User ID capability
       //    user_group = 1 means to provide this capability
       //    user_group = 0 means do not provide this capability

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'K3S_1010  USER_GROUP'
                fetch first row only;
            if SQLState = SQLStateOk;
               user_group = taflag1;
            else;
               user_group = 0;
            endif;

       // <<CH/BW>>
       //Specia table code record only for Bindley/Cardinal
       //    rx_otc = 1 means execute special Bindley/Cardinal code
       //    rx_otc = 0 means do not execute special Bindley/Cardinal code

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'QQQ' and
                      ta_codeval = 'LC_FBMAX  RX_OTC    '
                fetch first row only;
            if SQLState = SQLStateOk;
               rx_otc   = taflag1;
            else;
               rx_otc   = 0;
            endif;

       // <<CORE-MARK>>
       //Specia table code record only for Core-Mark TRACS
       //    qc1_on = 1 means execute special Core-Mark code
       //    qc1_on = 0 means do not execute special Core-Mark code

            exec sql
              select count(*)
                into :CoreMarkCount
                from k_tablcod
                where ta_comp = ' ' and
                      ta_codetyp = 'TYP' and
                      ta_codeval = 'QC1';
            if CoreMarkCount > 0;
               qc1_on = 1;
            else;
               qc1_on = 0;
            endif;

       //car count logic
       //    CarLogic = 1 means car count logic being used
       //    CarLogic = 0 means car count logic not being used

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'K3S_1500  CAR_COUNT '
                fetch first row only;
            if SQLState = SQLStateOk and taflag1 = 1;
               CarLogic = 1;
            else;
               CarLogic = 0;
            endif;

       // Delay Fixed Cycle Suppliers until Due Date?
       //      DELAY_FC = 1 means yes, do delay Fixed Cycle orders until exact date
       //      DELAY_FC = 0 means no, do NOT delay, prompt early if requirement exists

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'K3S_1500  DELAY_FC  '
                fetch first row only;
            if SQLState = SQLStateOk and taflag1 = 1;
               DELAY_FC = 1;
            else;
               DELAY_FC = 0;
            endif;

       // Delay Varialble Order Cycle Suppliers until Due Date?
       //      DELAY_VOC = 1 means yes, do delay VOC supplier orders until exact date
       //      DELAY_VOC = 0 means no, do NOT delay, prompt early if requirement exists

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'K3S_1500  DELAY_VOC '
                fetch first row only;
            if SQLState = SQLStateOk and taflag1 = 1;
               DELAY_VOC = 1;
            else;
               DELAY_VOC = 0;
            endif;

       // Manual Min/Max sequence reversed ?
       //      ManMinMaxR = 1 means yes, calculate Days 1st, Units 2nd
       //      ManMinMaxR = 0 means no, leave it at Units 1st, Days 2nd

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'K3S_C131  REVERSE   '
                fetch first row only;
            if SQLState = SQLStateOk and taflag1 = 1;
               ManMinMaxR = 1;
            else;
               ManMinMaxR = 0;
            endif;

       //Pitc Foods logic being used?
       //    Pitco_Log = 1 means Pitco Foods logic being used
       //    Pitco_log = 0 means standard K3S logic in use
       //      Pitco_typ = 0 means original Pitco Foods logic being used
       //      Pitco_typ = 1 means K3S SS value, but use a Max SS cap

            Pitco_Log = 0;
            exec sql
              select ta_flag1, ta_flag2, ta_number1, ta_number2, ta_number3
                into :taflag1, :taflag2, :tanumber1, :tanumber2, :tanumber3
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'PITCO               '
                fetch first row only;
            if SQLState = SQLStateOk;
               re_calc_ss = 1;
               pit_min = 01;
               pit_max = 05;
               if taflag1 = 1;

                  Pitco_typ = 0;
                  Pitco_SScap = 0;
                  if taflag2 = 1;
                     Pitco_typ = 1;
                     Pitco_SScap = tanumber1;
                     if Pitco_SScap < 10;
                        Pitco_SScap = 10;
                     endif;
                  endif;

                  Pitco_Log = 1;
                  if tanumber2 > pit_min;
                     pit_min = tanumber2;
                  endif;
                  if tanumber3 <> pit_max;
                     pit_max = tanumber3;
                  endif;
               else;
                  Pitco_Log = 0;
               endif;
            endif;

       //produc group service level target minimum logic
       //    G1_Logic = 1 means product group service level record being used
       //    G1_Logic = 0 means product group service level record NOT being used,
       //                 which also means that our original Buy Group method in use

            exec sql
              select ta_flag1
                into :taflag1
                from k_tablcod
                where ta_comp = :pr_comp and
                      ta_codetyp = 'APP' and
                      ta_codeval = 'K3S_1500  G1_MINSVC '
                fetch first row only;
            if SQLState = SQLStateOk and taflag1 = 1;
               G1_Logic = 1;
            else;
               G1_Logic = 0;
            endif;

       // <<CH/BW>>
       //automate add days. in this section we are trying to determine
       //    if any records exist at all in any of the 5 different sections,
       //     L1 Location - all products
       //     P1 Product - all locations
       //     P2 Product - this location
       //     S1 Supplier - all locations
       //     S2 Supplier - this location
       //  do any location L1 records exist?

            exec sql
               select count(*)
                 into :L1_count
                 from k_autoadd
                 where aa_comp = :pr_comp and
                       aa_rectype = 'L1';

            if L1_count > 0;
                L1_add_day = *on;
            endif;
       //  do any product P1 records exist?

            exec sql
               select count(*)
                 into :P1_count
                 from k_autoadd
                 where aa_comp = :pr_comp and
                       aa_rectype = 'P1';

            if P1_count > 0;
               P1_add_day = *on;
            endif;
       //  do any product P2 records exist?

            exec sql
               select count(*)
                 into :P2_count
                 from k_autoadd
                 where aa_comp = :pr_comp and
                       aa_rectype = 'P2';

            if P2_count > 0;
               P2_add_day = *on;
            endif;
       //  do any supplier S1 records exist?

            exec sql
               select count(*)
                 into :S1_count
                 from k_autoadd
                 where aa_comp = :pr_comp and
                       aa_rectype = 'S1';

            if S1_count > 0;
               S1_add_day = *on;
            endif;
       //  do any supplier S2 records exist?

            exec sql
               select count(*)
                 into :S2_count
                 from k_autoadd
                 where aa_comp = :pr_comp and
                       aa_rectype = 'S2';

            if S2_count > 0;
               S2_add_day = *on;
            endif;

         endif;

       //---------------------------------------------------- Location break

       //change in location
         if pr_locn <> locn;
            locn    = pr_locn;

       //get location
            exec sql
              select *
                into :locatns_rec
                from k_locatns
                where lc_comp = :pr_comp and
                      lc_locn = :pr_locn
                fetch first row only;
            if SQLState = SQLStateOk;

       //    calculate deal window
               dealwindow = lc_sysdate + %days(lc_dwindow);
       // <<CH/BW>>
               evalr test_stop = lc_desc;

       // determine day of week for variable order cycle
               Weeks = %diff(lc_sysdate:d'1899-12-30':*days);
               DayNbr = %rem(Weeks:7);
               if DayNbr = 0;
                  DayNbr = 7;
               endif;

               if #do_once <> 1;
                  #do_once = 1;

       // Convert alpha field to date
                  today = lc_sysdate;

       // retrieve first day of next period
       //   do this only once, for all 3 forecasting period types
                  callp K3S_M160(pr_comp:
                                 today:
                                 first12:
                                 first13:
                                 first52);

       // calculate number of days from today until first day next period
       // convert date to alpha
                  u1sttest = %char(first12:*iso);
                  if u1sttest <> '0001-01-01';
                     @u_diff12 = %diff(first12:today:*days);
                  endif;

                  if u1sttest <> '0001-01-01';
                     @u_diff13 = %diff(first13:today:*days);
                  endif;

                  u1sttest = %char(first52:*iso);
                  if u1sttest <> '0001-01-01';
                     @u_diff52 = %diff(first52:today:*days);
                  endif;

               endif;
            endif;

         endif;

       //----------------------------------------  Buy group/ location break

       //change in buy group/location

         prbuyr = pr_buyr;
         prlocn = pr_locn;

         if buyrlocn <> buyrlocnsv;
            buyrlocnsv = buyrlocn;

       //get buy group/location record
            exec sql
              select bs_minsvc
                into :bsminsvc
                from k_buyrsrv
                where bs_comp = :pr_comp and
                      bs_buyr = :pr_buyr and
                      bs_locn = :pr_locn
                fetch first row only;
            if SQLState = RowNotFound;
       //if no record exists, use default of .980
               bsminsvc = .980;
            endif;

       // <<CORE-MARK>>
       //Specia logic to determine if this chain needs bump test
       //    qc1_chain = 1 means this chain to use special Core-Mark logic
       //    qc1_chain = 0 means this chain does not use special logic
            if qc1_on = 1;

               exec sql
                 select ta_flag1
                   into :taflag1
                   from k_tablcod
                   where ta_comp = :pr_comp and
                         ta_codetyp = 'QC1' and
                         ta_codeval = :pr_buyr
                   fetch first row only;
               if SQLState = SQLStateOk;
                  qc1_chain = taflag1;
               else;
                  qc1_chain = 0;
               endif;

            endif;

         endif;

       //---------------------------------------------------- Supplier break

       //change in supplier ID
         #prlocn = pr_locn;
         #prsupl = pr_supl;
         #prsuplsub = pr_suplsub;

         if supl <> suplsaved;

       //save new supplier id
            suplsaved = supl;

       //writ supplier soq record, if not first cycle
            if #notfirst = *on;

               #tstcur1val = #cur1val;
               #tstcur1unt = #cur1unt;

               exsr $_writesoq;
            endif;

       //set on indicator for 'not first' cycle
            #notfirst = *on;

       //get supplier
            exsr $_getsupl;

       //day time remove previous orders
            if (day_time = 1) and
               (product = *blanks);
               exsr $_rmvsoq;
            endif;

       //clear supplier soq record
            exsr $_clrsuplq;

         endif;

       //-------------------------------------------------- Process products

       //process product record

       //determin if product is active during the nite job
       //    product should begin delete count, or be removed
         if (day_time = 0) AND                   //night
            (pr_lstintr <> lc_sysdate); //last interface date <> locn system date

       //    increment delete counter
            pr_deltcnt += 1;
            lg_deltcnt = pr_deltcnt;

       //    remove product
            if pr_deltcnt > lc_deltlmt;  //delete count days

               exec sql
                 delete k_product
                   where current of prcursor;

       //     put entry into log file for removal of product
               lg_logtype = '4';
               lda_rctyp4 += 1;
               exsr $_log_prod;

       //    update product delete counter
            else;
       //update product delete counter
               exec sql
                 update k_product
                 set pr_deltcnt = :pr_deltcnt
                 where current of prcursor;

       //     put entry into log file for counting days without update
               lg_logtype = '3';
               lda_rctyp3 += 1;
               exsr $_log_prod;

            endif;

       //continue product processing
         else;

       //only process products with valid date
            if pr_lstintr = lc_sysdate;

       //-- start develop hold out quantity if regular source order
               if alt_sour = 0;
                  pr_qtyhold = 0;

       //     generate key for testing
       //        combined supplier
                  if sp_altsrce = 2;
                     hold_locn  = pr_cmblocn;
                     hold_supl  = pr_cmbsupl;
                     hold_sub   = pr_cmbsub;

       //        regular supplier
                  else;
                     hold_locn  = pr_locn;
                     hold_supl  = pr_supl;
                     hold_sub   = pr_suplsub;
                  endif;

       //     first test, does hold out quantity exist?
       //       first test, does hold out quantity exist?
                  exec sql
                    select count(*)
                      into :prodhld_count
                      from k_prodhld
                      where pu_comp = :pr_comp and
                            pu_locn = :hold_locn and
                            pu_supl = :hold_supl and
                            pu_suplsub = :hold_sub and
                            pu_prod = :pr_prod;

       //     if yes, then see if in window
                   if prodhld_count > 0;

                      exsr InzInpSrchpu;
    ‚   //initialize StmtString
                      exsr IntSQLStmtpu;
    ‚   //prepare statement
    ‚                  exsr PrepDynSQLStmtpu;

                      if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                         exsr opnpucursor;

                         dow SQLState = SQLStateOk;

                            exec sql
                              fetch next
                                 from pucursor
                                 into :prodhld_rec;

                            if SQLState = RowNotFound;
                               leave;
                            endif;

                            if SQLState = SQLStateOk;

       //        if in window, accumlate hold out quantity
                               if pu_begin  <=  lc_sysdate AND
                                  pu_end    >=  lc_sysdate;

                                  pr_qtyhold += pu_hldqty;
                               endif;
                            endif;
                         enddo;
                         exsr clspucursor;
                      endif;
                   endif;
               endif;
       //-- end   develop hold out quantity if regular source order

       //clear andinitialize product soq record and product work fields
               exsr $_clrprodq;

       // <<CH/BW>>
       //-- start develop automated add days if regular source order
               if alt_sour = 0;

       // test to see if this location has been stopped from building
       // extra days. the last 4 letters of the location description
       // field would contain 'STOP' if this location should be stopped.
                  if test_STOP <> 'STOP';

       // L1 section - automated add days
                     if L1_add_day = *on;

                        exsr InzInpSrchaa1;

                        exsr IntSQLStmtaa;

                        exsr PrepDynSQLStmtaa1;

                        if SQLState = SQLStateOk;

                           add_rectyp = 'L1';
                           if sp_altsrce = 2;
                              add_locn  = pr_cmblocn;
                           else;
                              add_locn  = pr_locn;
                           endif;
                           add_supl  = *blanks;
                           add_sub   = *blanks;
                           add_prod  = *blanks;

                           exsr opnaacursor1;

                           dow SQLState = SQLStateOk;

                             exec sql
                                fetch next
                                  from aacursor1
                                  into :autoadd_rec;

                             if SQLState = RowNotFound;
                                leave;
                             endif;
       //     if in window, accumlate automated add days
                              if aa_begin  <=  lc_sysdate AND
                                  aa_end    >=  lc_sysdate;
                                  if pq_add_day + aa_add_day < 500;
                                     pq_add_day += aa_add_day;
                                  endif;
                              endif;

                           enddo;
                           exsr clsaacursor1;
                        endif;
                     endif;
       //      L1_add_day = *on

       // P1 section - automated add days
                     if P1_add_day = *on;

                        exsr InzInpSrchaa2;

                        exsr IntSQLStmtaa;

                        exsr PrepDynSQLStmtaa2;

                        if SQLState = SQLStateOk;

                           add_rectyp = 'P1';
                           if sp_altsrce = 2;
                              add_supl  = pr_cmbsupl;
                              add_sub   = pr_cmbsub;
                           else;
                              add_supl  = pr_supl;
                              add_sub   = pr_suplsub;
                           endif;
                           add_locn  = *blanks;
                           add_prod  = pr_prod;

                           exsr opnaacursor2;

                           dow SQLState = SQLStateOk;

                             exec sql
                                fetch next
                                  from aacursor2
                                  into :autoadd_rec;

                             if SQLState = RowNotFound;
                                leave;
                             endif;
       //     if in window, accumlate automated add days

                             if aa_begin  <=  lc_sysdate AND
                                aa_end    >=  lc_sysdate;

                                if pq_add_day + aa_add_day < 500;
                                   pq_add_day += aa_add_day;
                                endif;
                             endif;

                           enddo;
                           exsr clsaacursor2;

                        endif;
                     endif;
       //      P1_add_day = *on

       // P2 section - automated add days
                     if P2_add_day = *on;

                        exsr InzInpSrchaa2;

                        exsr IntSQLStmtaa;

                        exsr PrepDynSQLStmtaa2;

                        if SQLState = SQLStateOk;

                           add_rectyp = 'P2';
                           if sp_altsrce = 2;
                              add_locn  = pr_cmblocn;
                              add_supl  = pr_cmbsupl;
                              add_sub   = pr_cmbsub;
                           else;
                              add_locn  = pr_locn;
                              add_supl  = pr_supl;
                              add_sub   = pr_suplsub;
                           endif;
                           add_prod  = pr_prod;

                           exsr opnaacursor2;

                           dow SQLState = SQLStateOk;

                              exec sql
                                 fetch next
                                   from aacursor2
                                   into :autoadd_rec;

                              if SQLState = RowNotFound;
                                 leave;
                              endif;

                              if aa_begin  <=  lc_sysdate AND
                                 aa_end    >=  lc_sysdate;
                                 if pq_add_day + aa_add_day < 500;
                                    pq_add_day += aa_add_day;
                                 endif;
                              endif;

                           enddo;
                           exsr clsaacursor2;
                        endif;
                     endif;
       //      P2_add_day = *on

       // S1 section - automated add days
                     if S1_add_day = *on;

                        exsr InzInpSrchaa2;

                        exsr IntSQLStmtaa;

                        exsr PrepDynSQLStmtaa2;

                        if SQLState = SQLStateOk;

                           add_rectyp = 'S1';
                           if sp_altsrce = 2;
                              add_supl  = pr_cmbsupl;
                              add_sub   = pr_cmbsub;
                           else;
                              add_supl  = pr_supl;
                              add_sub   = pr_suplsub;
                           endif;
                           add_locn  = *blanks;
                           add_prod  = *blanks;

                           exsr opnaacursor2;

                           dow SQLState = SQLStateOk;

                              exec sql
                                 fetch next
                                   from aacursor2
                                   into :autoadd_rec;

                              if SQLState = RowNotFound;
                                 leave;
                              endif;

       //     if in window, accumlate automated add days
                              if aa_begin  <=  lc_sysdate AND
                                 aa_end    >=  lc_sysdate;
                                 if  pq_add_day + aa_add_day < 500;
                                     pq_add_day += aa_add_day;
                                 endif;
                              endif;

                           enddo;
                           exsr clsaacursor2;
                        endif;
                     endif;
       //      S1_add_day = *on

       // S2 section - automated add days
                     if S2_add_day = *on;

                        exsr InzInpSrchaa2;

                        exsr IntSQLStmtaa;

                        exsr PrepDynSQLStmtaa2;

                        if SQLState = SQLStateOk;

                           add_rectyp = 'S2';
                           if sp_altsrce = 2;
                              add_locn  = pr_cmblocn;
                              add_supl  = pr_cmbsupl;
                              add_sub   = pr_cmbsub;
                          else;
                              add_locn  = pr_locn;
                              add_supl  = pr_supl;
                              add_sub   = pr_suplsub;
                          endif;
                          add_prod  = *blanks;

                          exsr opnaacursor2;

                          dow SQLState = SQLStateOk;

                              exec sql
                                 fetch next
                                   from aacursor2
                                   into :autoadd_rec;

                              if SQLState = RowNotFound;
                                 leave;
                              endif;

       //     if in windwaccumlate automated add days
                              if aa_begin  <=  lc_sysdate AND
                                 aa_end    >=  lc_sysdate;
                                 if pq_add_day + aa_add_day < 500;
                                    pq_add_day += aa_add_day;
                                 endif;
                              endif;

                          enddo;
                          exsr clsaacursor2;
                        endif;
                     endif;
       //      S2_add_day = *on

                  endif;
       //   'STOP' automated add days logic

               endif;
       //-- end   develop automated add days if regular source order

       //------- develop quantity balance start

       //nclude customer back orders from night job interface file, and
       //       hold out quantity and transfer pending quantity
               pq_qtybsum = pq_qtybsum + pr_qtyback
                                       + pr_qtyhold
                                       + pr_qtypend;

       //includ promotional quantity, if within dates
               if (pr_prombeg <= lc_sysdate) AND
                  (pr_promend >= lc_sysdate);
                  pq_qtybsum += pr_promqty;
               endif;

       //calculate balance
               if pr_qtyohnd
                  + pr_qtyoord
                  - pq_qtybsum <= 9999999;

                  pq_qtybaln = pr_qtyohnd
                             + pr_qtyoord
                             - pq_qtybsum;
               else;
                  pq_qtybaln = 9999999;
               endif;

               pr_qtybaln = pq_qtybaln;

       //------- develop quantity balance end

       //ready for prodsoq record
               pq_qtybaln = pr_qtybaln;

       //if day time and regular order (not alternate source supplier)
       //            clear soq and date
               if (day_time = 1) AND   //1 = day
                  (alt_sour = 0);
                  clear pq_soqact;
                  clear pq_soqovrd;
               endif;
       //-------------------------------------------- Major categories begin
       //select the category of product to work with
               select;

       //                                           ----------- New products
       //new products
                  when pr_sysstat = 'N';
                       pq_chknew = 1;
                       so_chknew += 1;

       //                                           -------- Manual products
       //manual products accumulation
                  when pr_usrstat = 'M';
                       pq_chkmanl = 1;
                       so_chkmanl += 1;
       //                                           -- Discontinued products
       //discontinued products accumulation
                  when pr_sysstat = 'D';
                       pq_chkdisc = 1;
                       so_chkdisc += 1;

       // if balance is negative, AND flag is set to buy disc products,
       //    then purchase the negative amount
                       if pr_qtybaln < 0 AND lc_purdisc = 1;

       //     make value positive
                          pq_soqact  = pr_qtybaln * -1;

       // if regular order (not alternate source order), get cost
                          if alt_sour = 0;
                             pq_costord = pr_costreg;

       //    see if flag is set for rounding of the balance
       //          values:  0  do not round
       //                   1  round up to cover total SOQ
       //                   2  round up or down
                             if lc_rnddisc > 0;
                                soqactw1 = pq_soqact / pr_buymult;
                                soqactw2 = %rem(pq_soqact:pr_buymult);

                                if soqactw1 > 0;
                                   least_1  = *on;
                                else;
                                   least_1  = *off;
                                endif;

                                if soqactw2 > 0;
                                   soqactw1 += 1;

                                   if least_1 = *on  AND  lc_rnddisc = 2;
                                      eval(h) soqactw1 = pq_soqact / pr_buymult;
                                   endif;

                                   pq_soqact = pr_buymult * soqactw1;
                                endif;

       // purchase minimum quantity test
                                if pq_soqact < pr_minqty;
                                   pq_soqact = pr_minqty;
                                endif;

                             endif;
                          endif;
                       endif;

       //                                           ------- No cost products
       //product must have cost for calculations
                  when pr_costeac = 0;

                       all_cost_0 += 1;

       //No cost products start ----------------
       //  only exception would be if buyer uses Manual Min/Max in UNITS
       //                       or if buyer uses Manual Min/Max in DAYS

       //  only execute if a manual minimum UNITS is used
       //            or if a manual minimum DAYS  is used
                       if pr_maminiu > 0 OR
                          pr_maminid > 0;

                          all_minmax += 1;

                          @m_seasonl = pr_seasonl;
                          @m_forcast = pr_forcast;
                          @m_forcper = pr_forcper;
                          @m_forcint = pr_forcint;
                          @m_longtrm = pr_longtrm;

                          @m_altsour = alt_sour;
                          @m_maminiu = pr_maminiu;
                          @m_mamaxiu = pr_mamaxiu;
                          @m_maminid = pr_maminid;
                          @m_mamaxid = pr_mamaxid;
                          @m_qtybaln = pr_qtybaln;
                          @m_chkopnt = pq_chkopnt;
                          @m_sochk   = so_chkopnt;

                          @m_minday  = 0;
                          @m_maxday  = 0;
                          @m_minunit = pr_maminiu;
                          @m_maxunit = pr_mamaxiu;
                          @m_mnwork  = 0;
                          @m_mxwork  = 0;

                          if pr_maminid > 0;
                             @m_maminid = 0;
                             @m_mamaxid = 0;
                             eval(h) @m_maminiu = pr_maminid *
                                               ((pr_forcast * pr_forcint)/364);
                             eval(h) @m_mamaxiu = pr_mamaxid *
                                               ((pr_forcast * pr_forcint)/364);
                          endif;

       // call subroutine to calculate manual minimum or maximum values

                          callp K3S_Calc_Manual(@m_altsour:
                                                @m_maminid:
                                                @m_mamaxid:
                                                @m_maminiu:
                                                @m_mamaxiu:
                                                @m_qtybaln:
                                                @m_chkopnt:
                                                @m_sochk:
                                                @m_seasonl:
                                                @m_forcast:
                                                @m_forcper:
                                                @m_forcint:
                                                @m_longtrm:
                                                @m_minday:
                                                @m_maxday:
                                                @m_minunit:
                                                @m_maxunit:
                                                @m_mnwork:
                                                @m_mxwork:
                                                @m_str_day:
                                                @m_str_unt:
                                                @u_comp:
                                                @u_days:
                                                @u_dowmap:
                                                @u_dowper1:
                                                @u_dowper2:
                                                @u_dowper3:
                                                @u_dowper4:
                                                @u_dowper5:
                                                @u_dowper6:
                                                @u_dowper7:
                                                @u_sstper1:
                                                @u_sstper2:
                                                @u_sstper3:
                                                @u_sstper4:
                                                @u_sstper5:
                                                @u_sstper6:
                                                @u_sstper7:
                                                @u_diff12:
                                                @u_diff13:
                                                @u_diff52:
                                                @u_use:
                                                @u_Ds_flag:
                                                @u_DayNbr:
                                                @u_ix:
                                                @u_DS_Days:
                                                @u_DS_wdis:
                                                @u_DS_dlyu:
                                                @u_DS_spct:
                                                @u_DS_susg:
                                                @u_DS_accu:
                                                @u_DS_prno:
                                                @u_DS_fctr:
                                                @u_DS_warn);
       //                 exsr $_manual;

                          minday     = @m_minday;
                          maxday     = @m_maxday;
                          minunit    = @m_minunit;
                          maxunit    = @m_maxunit;
                          mnwork     = @m_mnwork;
                          mxwork     = @m_mxwork;
                          pq_chkopnt = @m_chkopnt;
                          so_chkopnt = @m_sochk;

       //if maximum units less than minimum units, make equal
                          if maxunit < minunit;
                             maxunit = minunit;
                             mxwork  = mnwork;
                          endif;

       //save data into fields for later update
                          pq_opointu = minunit;
                          pq_oruptou = maxunit;

                          clear pq_soqact;

       //calculate order quantity if balance falls below order point
                          if pq_qtybaln < pq_opointu;

                             @q_maxunit = pq_oruptou;
                             @q_minunit = pq_opointu;
                             @q_qtybaln = pq_qtybaln;
                             @q_minqty  = pr_minqty;
                             @q_buymult = pr_buymult;
                             @q_convpak = pr_convpak;
                             @q_convpkp = pr_convpkp;


                             callp K3S_Calc_SOQ(@q_maxunit:
                                                @q_minunit:
                                                @q_qtybaln:
                                                @q_minqty:
                                                @q_buymult:
                                                @q_convpak:
                                                @q_convpkp:
                                                @q_soq);
       //                    exsr $_calc_SOQ;

                             pq_soqact = @q_soq;
                          endif;

                       endif;
       //No cost products end ------------------

       //                                            --- All normal products
       //This section includes all other products not selected above,
       //     which would be all normal products to be calculated.
                  other;

       //calculate stock out values

       // for alternate source orders:
       //         use regular source order cycle,
       //             which came from program k3s_9110 in field pr_daysunt,
       //             and will be passed to field pq_orcycle
       //         use regular product buy multiple, and save alternate
       //             source buy multiple
                       if alt_sour = 1;
                          sp_orcycle = pr_daysunt;
                          pq_orcycle = pr_daysunt;
                          savbuymult = pr_buymult;
                          pr_buymult = pr_buymulo;
                       endif;

       // determine lead time to be used

       // for regular orders (not alternate source orders),
       // if product lead time less than supplier lead time, use supplier lt
       // unless flag set that doesn't allow this to happen
                       if alt_sour = 0 AND lc_ltmuse = 0 AND
                          pq_leadtm < sp_leadtmo;
                             pq_leadtm = sp_leadtmo;
                       endif;

       //----- Variable Order Cycle record for Lead Time - Begin
       // for regular orders (not alternate source orders),
       // where variable order cycle record is being used, and the lead
       // time use flag is on, use 'day of the week' lead time value
                       if alt_sour = 0   AND
                          vary_orcyc = 1 AND
                          si_leaduse = 1;
       //  D=Daily
                          if si_rectype = 'D';
                             select;
                                when DayNbr = 1;
                                     pq_leadtm  = si_leadt01;
                                     pq_leadtmv = si_leadv01;
                                when DayNbr = 2;
                                     pq_leadtm  = si_leadt02;
                                     pq_leadtmv = si_leadv02;
                                when DayNbr = 3;
                                     pq_leadtm  = si_leadt03;
                                     pq_leadtmv = si_leadv03;
                                when DayNbr = 4;
                                     pq_leadtm  = si_leadt04;
                                     pq_leadtmv = si_leadv04;
                                when DayNbr = 5;
                                     pq_leadtm  = si_leadt05;
                                     pq_leadtmv = si_leadv05;
                                when DayNbr = 6;
                                     pq_leadtm  = si_leadt06;
                                     pq_leadtmv = si_leadv06;
                                when DayNbr = 7;
                                     pq_leadtm  = si_leadt07;
                                     pq_leadtmv = si_leadv07;
                             endsl;
       //       just in case bad data gets into K_SUPLVOC field
                             if pq_leadtm  < 1;
                                pq_leadtm  = 1;
                             endif;
       //  pass PQ_LEADTM to SO_LEADTMO via SP_LEADTMO for K3S_1050 approval
                             sp_leadtmo = pq_leadtm;
                             if pq_leadtmv < 0;
                                pq_leadtmv = 0;
                             endif;
                          endif;
                       endif;
       //----- Variable Order Cycle record for Lead Time - End

       //----- Variable Order Cycle record for 'day of week' map  BEGIN
       // for regular orders (not alternate source orders),
       // where variable order cycle record is being used for 'D' Daily, and
       // use Weekly Distribution Values is 1=Yes, get the
       // percentages by day from Supplier record, or if Product record
       // exists use his values
                       if alt_sour = 0     AND
                          vary_orcyc = 1   AND
                          si_rectype = 'D' AND
                          @u_dowmap  = 1;

                          @u_dowper1 = si_dowper1;
                          @u_dowper2 = si_dowper2;
                          @u_dowper3 = si_dowper3;
                          @u_dowper4 = si_dowper4;
                          @u_dowper5 = si_dowper5;
                          @u_dowper6 = si_dowper6;
                          @u_dowper7 = si_dowper7;
                          @u_sstper1 = si_sstper1;
                          @u_sstper2 = si_sstper2;
                          @u_sstper3 = si_sstper3;
                          @u_sstper4 = si_sstper4;
                          @u_sstper5 = si_sstper5;
                          @u_sstper6 = si_sstper6;
                          @u_sstper7 = si_sstper7;
       //       check if there is a Product Weekly Distribution record

                          exec sql
                             select *
                               into :prodwkd_rec
                               from k_prodwkd
                               where pm_comp = :pr_comp and
                                     pm_locn = :pr_locn and
                                     pm_supl = :pr_supl and
                                     pm_suplsub = :pr_suplsub and
                                     pm_prod = :pr_prod
                               fetch first row only;

                          if SQLState = SQLStateOk;
                             @u_dowper1 = pm_dowper1;
                             @u_dowper2 = pm_dowper2;
                             @u_dowper3 = pm_dowper3;
                             @u_dowper4 = pm_dowper4;
                             @u_dowper5 = pm_dowper5;
                             @u_dowper6 = pm_dowper6;
                             @u_dowper7 = pm_dowper7;
                             @u_sstper1 = pm_sstper1;
                             @u_sstper2 = pm_sstper2;
                             @u_sstper3 = pm_sstper3;
                             @u_sstper4 = pm_sstper4;
                             @u_sstper5 = pm_sstper5;
                             @u_sstper6 = pm_sstper6;
                             @u_sstper7 = pm_sstper7;

       //  no Product specific safety stock, default to Supplier
                             if pm_sstper1 = 0 and
                                pm_sstper2 = 0 and
                                pm_sstper3 = 0 and
                                pm_sstper4 = 0 and
                                pm_sstper5 = 0 and
                                pm_sstper6 = 0 and
                                pm_sstper7 = 0;

                                @u_sstper1 = si_sstper1;
                                @u_sstper2 = si_sstper2;
                                @u_sstper3 = si_sstper3;
                                @u_sstper4 = si_sstper4;
                                @u_sstper5 = si_sstper5;
                                @u_sstper6 = si_sstper6;
                                @u_sstper7 = si_sstper7;
                             endif;

                          endif;
                       endif;
       //----- Variable Order Cycle record for 'day of week' map  END

       //calculate safety stock sum of elements
                       safesum = lc_repcary +
                                 pq_linecst +
                                 sp_orcycle +
                                 pr_forcint +
                                 pr_minqty  +
                                 pr_buymult +
                                 pr_costeac +
                                 pr_forcast +
                                 pr_fordevp +
                                 pr_service +
                                 pq_leadtm  +
                                 pq_leadtmv +
                                 pr_formeth +
                                 vary_orcyc +
                                 @u_dowmap;

       //if sum of elements has changed or if force of safety stock has been
       //      requested, then re-calculate safety stock
                       if (pr_safesum <> safesum) or
                          (re_calc_ss = 1);

                          pr_safesum =  safesum;

                          clear @s_sstimef;
                          clear @s_otimfac;
                          clear @s_devtime;
                          clear @s_intrval;

                          @s_repcary = lc_repcary * .01;
                          @s_linecst = pq_linecst;
                          @s_orcycle = sp_orcycle;
                          @s_forcint = pr_forcint;
                          @s_buymult = pr_buymult;
                          @s_minqty  = pr_minqty;
                          @s_costeac = pr_costeac;
                          @s_forcast = pr_forcast;
                          @s_fordevp = pr_fordevp * .01;
                          @s_service = pr_service * .01;
                          @s_leadtm  = pq_leadtm;
                          @s_leadtmv = pq_leadtmv * .01;
                          @s_formeth = pr_formeth;

       //call subprocedure to calculate safety stock

                          callp K3S_Calc_Safety(@s_repcary:
                                                @s_linecst:
                                                @s_orcycle:
                                                @s_forcint:
                                                @s_buymult:
                                                @s_minqty:
                                                @s_costeac:
                                                @s_forcast:
                                                @s_fordevp:
                                                @s_service:
                                                @s_leadtm:
                                                @s_leadtmv:
                                                @s_formeth:
                                                @s_sstimef:
                                                @s_otimfac:
                                                @s_devtime:
                                                @s_intrval:
                                                meandevary:
                                                strddevary);
       //                 exsr $_safety;

                          pr_sstimef = @s_sstimef;
                          pr_otimfac = @s_otimfac;
                          pr_devtimf = @s_devtime;
                          pr_intrval = @s_intrval;

                       endif;

       //if Pitco Fodds logic being used
       //  don't use regular Safety Stock Days logic for Pitco Foods
       //  but use their calculations
                       if Pitco_Log  = 1;
                          if Pitco_typ = 0;
                             pit_perAlp = %subst(pr_whslocn:8:3);
                             pit_percnt = %dec(%subst(pit_perAlp:1:2):2:2);
                             pit_whole = 0;
                             if  pit_percnt <> .30 and pit_percnt <> .50;
                                 pit_percnt =  .30;
                             endif;
                             eval(h) pit_whole = pq_leadtm * pit_percnt;
                             if pit_whole < pit_min;
                                pit_whole = pit_min;
                             endif;
                             if pit_whole > pit_max and pit_percnt = .30;
                                pit_whole = pit_max;
                             endif;
                             @s_sstimef = pit_whole;
                             pr_sstimef = pit_whole;
                             pr_safesum = 0;
                          else;
       //  K3S SS calc's with Pitco SS cap
                             if @s_sstimef > pitco_SScap and
                                @s_sstimef - pitco_SScap > .49;

                                @s_sstimef = pitco_SScap;
                                pr_sstimef = pitco_SScap;
                                pr_safesum = 0;
                             endif;
                          endif;
                       endif;

       //if using weekly distribution logic,
       //  don't use regular Safety Stock Days logic (use SS Days of 0),
       //  only buy Order Cycle and Lead Time but nothing additional
       //    because of economics
                       if vary_orcyc = 1 AND
                          @u_dowmap  = 1;
                          @s_sstimef = 0;
                          pr_sstimef = 0;
                          @s_intrval = 0;
                          pr_intrval = 0;
                       endif;

       //if alternate source order, use order cycle as order time factor
       //**  if alt_sour = 1
       //**     pr_otimfac = sp_orcycle
       //**  endif

       //if alternate source order, restore buy multiple
                       if alt_sour = 1;
                          pr_buymult = savbuymult;
                       endif;

       //get deasonal profile information

       //   get profile factors, if product contains seasonal profile ID


                       prodsez_found = *off;
                       if pr_seasonl <> *blanks;
                            exec sql
                              select pz_factr01, pz_factr02, pz_factr03,
                                     pz_factr04, pz_factr05, pz_factr06,
                                     pz_factr07, pz_factr08, pz_factr09,
                                     pz_factr10, pz_factr11, pz_factr12,
                                     pz_factr13, pz_factr14, pz_factr15,
                                     pz_factr16, pz_factr17, pz_factr18,
                                     pz_factr19, pz_factr20, pz_factr21,
                                     pz_factr22, pz_factr23, pz_factr24,
                                     pz_factr25, pz_factr26, pz_factr27,
                                     pz_factr28, pz_factr29, pz_factr30,
                                     pz_factr31, pz_factr32, pz_factr33,
                                     pz_factr34, pz_factr35, pz_factr36,
                                     pz_factr37, pz_factr38, pz_factr39,
                                     pz_factr40, pz_factr41, pz_factr42,
                                     pz_factr43, pz_factr44, pz_factr45,
                                     pz_factr46, pz_factr47, pz_factr48,
                                     pz_factr49, pz_factr50, pz_factr51,
                                     pz_factr52
                                into :pzfactr01, :pzfactr02,: pzfactr03,
                                     :pzfactr04, :pzfactr05, :pzfactr06,
                                     :pzfactr07, :pzfactr08, :pzfactr09,
                                     :pzfactr10, :pzfactr11, :pzfactr12,
                                     :pzfactr13, :pzfactr14, :pzfactr15,
                                     :pzfactr16, :pzfactr17, :pzfactr18,
                                     :pzfactr19, :pzfactr20, :pzfactr21,
                                     :pzfactr22, :pzfactr23, :pzfactr24,
                                     :pzfactr25, :pzfactr26, :pzfactr27,
                                     :pzfactr28, :pzfactr29, :pzfactr30,
                                     :pzfactr31, :pzfactr32, :pzfactr33,
                                     :pzfactr34, :pzfactr35, :pzfactr36,
                                     :pzfactr37, :pzfactr38, :pzfactr39,
                                     :pzfactr40, :pzfactr41, :pzfactr42,
                                     :pzfactr43, :pzfactr44, :pzfactr45,
                                     :pzfactr46, :pzfactr47, :pzfactr48,
                                     :pzfactr49, :pzfactr50, :pzfactr51,
                                     :pzfactr52
                                from k_prodsez
                                where pz_comp = :pr_comp and
                                      pz_seasonl = :pr_seasonl
                                fetch first row only;
                                if SQLState = SQLStateOk;
                                   prodsez_found = *on;
                                endif;
                       endif;

       //   set factors to 1.00 if:

       //        1) profile not found for this product, or
       //        2) no profile for this product, and long term trend <> 1.00
       //        3) no profile for this product, and Weekly Distribution ON

                       if ((pr_seasonl <> *blanks) and
                            not prodsez_found)
                               or
                          ((pr_seasonl = *blanks) and
                           (pr_longtrm <> 1.00))
                               or
                          ((pr_seasonl = *blanks) and
                           (@u_dowmap = 1));

                          pzfactr01 = 1.00;
                          pzfactr02 = 1.00;
                          pzfactr03 = 1.00;
                          pzfactr04 = 1.00;
                          pzfactr05 = 1.00;
                          pzfactr06 = 1.00;
                          pzfactr07 = 1.00;
                          pzfactr08 = 1.00;
                          pzfactr09 = 1.00;
                          pzfactr10 = 1.00;
                          pzfactr11 = 1.00;
                          pzfactr12 = 1.00;
                          pzfactr13 = 1.00;
                          pzfactr14 = 1.00;
                          pzfactr15 = 1.00;
                          pzfactr16 = 1.00;
                          pzfactr17 = 1.00;
                          pzfactr18 = 1.00;
                          pzfactr19 = 1.00;
                          pzfactr20 = 1.00;
                          pzfactr21 = 1.00;
                          pzfactr22 = 1.00;
                          pzfactr23 = 1.00;
                          pzfactr24 = 1.00;
                          pzfactr25 = 1.00;
                          pzfactr26 = 1.00;
                          pzfactr27 = 1.00;
                          pzfactr28 = 1.00;
                          pzfactr29 = 1.00;
                          pzfactr30 = 1.00;
                          pzfactr31 = 1.00;
                          pzfactr32 = 1.00;
                          pzfactr33 = 1.00;
                          pzfactr34 = 1.00;
                          pzfactr35 = 1.00;
                          pzfactr36 = 1.00;
                          pzfactr37 = 1.00;
                          pzfactr38 = 1.00;
                          pzfactr39 = 1.00;
                          pzfactr40 = 1.00;
                          pzfactr41 = 1.00;
                          pzfactr42 = 1.00;
                          pzfactr43 = 1.00;
                          pzfactr44 = 1.00;
                          pzfactr45 = 1.00;
                          pzfactr46 = 1.00;
                          pzfactr47 = 1.00;
                          pzfactr48 = 1.00;
                          pzfactr49 = 1.00;
                          pzfactr50 = 1.00;
                          pzfactr51 = 1.00;
                          pzfactr52 = 1.00;
                       endif;

                       @u_comp    = pr_comp;
                       @u_seasonl = pr_seasonl;
                       @u_forcast = pr_forcast;
                       @u_forcper = pr_forcper;
                       @u_forcint = pr_forcint;
        //             @u_sysdate = %char(lc_sysdate:*iso);
                       @u_longtrm = pr_longtrm;

                       clear @u_use;
       //              @u_days = pr_otimfac;
                       @u_days = sp_orcycle;

                       callp K3S_Calc_Usage(@u_comp:
                                            @u_seasonl:
                                            @u_days:
                                            @u_forcast:
                                            @u_forcper:
                                            @u_forcint:
                                            @u_longtrm:
                                            @u_dowmap:
                                            @u_dowper1:
                                            @u_dowper2:
                                            @u_dowper3:
                                            @u_dowper4:
                                            @u_dowper5:
                                            @u_dowper6:
                                            @u_dowper7:
                                            @u_sstper1:
                                            @u_sstper2:
                                            @u_sstper3:
                                            @u_sstper4:
                                            @u_sstper5:
                                            @u_sstper6:
                                            @u_sstper7:
                                            @u_diff12:
                                            @u_diff13:
                                            @u_diff52:
                                            @u_use:
                                            @u_Ds_flag:
                                            @u_DayNbr:
                                            @u_ix:
                                            @u_Ds_days:
                                            @u_Ds_wdis:
                                            @u_Ds_dlyu:
                                            @u_Ds_spct:
                                            @u_Ds_susg:
                                            @u_Ds_accu:
                                            @u_Ds_prno:
                                            @u_Ds_fctr:
                                            @u_Ds_warn);
       //              exsr $_usage;


       //calculate specified stock out
                       specout = (1 - (pr_service * .01)) *
                                       pr_costeac *
                                       @u_use;

       //Ifcustomer does not want products suggested prior to receiving,
       // then don't use stock out values
                       if  must_recv  = 1 and
                           pr_lstrcvd = not_recvd and
                           pr_qtyoord > 0;

                           specout = 0;
                       endif;

       //accumulatespecified stock out value for supplier order delay calc
       // if product has a future Exclude Buying Until date, then don't use
       // stock out values
                       if pr_excuntl > lc_sysdate;
                          if excl_buy = 1;
                             if pr_qtyback = 0;
                                specout = 0;
                             endif;
                          else;
                             specout = 0;
                          endif;
                       endif;
                       specoutt += specout;

       //save specified stock out value to disk
                       pq_soqsso = specout;

       //calculate expected stock out
                       time = 5;

       //set off flag for forward buy logic testing
                       backout = *off;

       //set off flag for being totally out of stock logic
                       total_out = '0';

                       exsr $_expected;

       //exclude forward bought products until forward date reached
                       if (alt_sour = 0);

                          if (backout = *on) and
                             (pr_fbuydat > lc_sysdate);

                             specoutt -= specout;
                             pq_soqsso = 0;
                          else;

                             expstkout(6) += exptout;

       //calculate expected stock outs for delays of 0,1,2,3,4 days
                             time = 0;
                             for z = 1 to 5;
                               exsr $_expected;
                               time = z;
                               expstkout(z) += exptout;
                             endfor;

       //If customer does not want products suggested prior to receiving,
       // then don't use stock out values
                             if must_recv  = 1 and
                                pr_lstrcvd = not_recvd and
                                pr_qtyoord > 0;

                                expstkout = 0;
                             endif;

       //accumulate expected stock out values for supplier order delay cals
       // if product has a future Exclude Buying Until date, then don't use
       // stock out values
                            if pr_excuntl > lc_sysdate;
                               if excl_buy = 1;
                                  if pr_qtyback = 0;
                                     expstkout = 0;
                                  endif;
                               else;
                                  expstkout = 0;
                               endif;
                            endif;
                            expstkoutt += expstkout;

       //save expected stock out values to disk
                            pq_soqeso1 = expstkout(1);
                            pq_soqeso2 = expstkout(2);
                            pq_soqeso3 = expstkout(3);
                            pq_soqeso4 = expstkout(4);
                            pq_soqeso5 = expstkout(5);
                            pq_soqeso6 = expstkout(6);

       //Enhancement when totally out of stock, yet the specified stock
       //   out value is greater than (or =) the expected stock out value.
       //   Set specified stock out just under expected stock out value,
       //   which will force OP check. Also correct the order totals,
       //   and set flag to remember we're totally out, so that the
       //   DELAY can be set to -5 further down in the code.
                            if pr_qtybaln <= 0 AND
                               specout >= expstkout(1) AND
                               specout > 0;

                               total_out = '1';
                               specoutt -= specout;
                               specout = expstkout(1) - .001;
                               pq_soqsso = specout;
                               specoutt += specout;
                            endif;

                          endif;

                       endif;

       //---------------------------------------------- calculate delay days
       //if no manual minimum in days or units being used, and product
       //   does have a specified stock out value, then perform order point
       //   check, service check, and delay days calculations
                       if (pr_maminid = 0) and
                          (pr_maminiu = 0) and
                          (specout > 0);

       //order point check
                          if (expstkout(1) > specout);
                             pq_chkopnt = 1;
                             so_chkopnt += 1;
                          endif;

       //service check (order point check for top items)
                          if G1_Logic = 0;
                             if (expstkout(1) > specout) and
                                pr_service >= bsminsvc;

                                pq_chkserv = 1;
                                so_chkserv += 1;
                             endif;
                          endif;

       //ervice check (order point check for top items)
       // using Product Group service level target records
                          if G1_Logic = 1;
       //         first test using both PR_GROUP1 and PR_GROUP2
                             wrkGroup2 = pr_group2;

                             exec sql
                               select g1_minsvc
                                 into :g1minsvc
                                 from k_grp1srv
                                 where g1_comp = :pr_comp and
                                       g1_locn = :pr_locn and
                                       g1_group1 = :pr_group1 and
                                       g1_group2 = :wrkGroup2
                                 fetch first row only;

                             if SQLState = SQLStateOk;
                                if (expstkout(1) > specout) and
                                   pr_service >= g1minsvc;

                                   pq_chkserv = 1;
                                   so_chkserv += 1;
                                endif;
                             else;
       //         second test assume only PR_GROUP1 used
                                wrkGroup2 = *blanks;

                                exec sql
                                  select g1_minsvc
                                    into :g1minsvc
                                    from k_grp1srv
                                    where g1_comp = :pr_comp and
                                          g1_locn = :pr_locn and
                                          g1_group1 = :pr_group1 and
                                          g1_group2 = :wrkGroup2
                                    fetch first row only;

                                if SQLState = SQLStateOk;

                                   if (expstkout(1) > specout) and
                                      pr_service >= g1minsvc;

                                      pq_chkserv = 1;
                                      so_chkserv += 1;
                                   endif;
                                endif;
                             endif;
                          endif;

       //calculate delay days for product
                          delay = 0;
                          foundit = *off;
                          i = 2;

                          dou (foundit = *on) or (i = 7);

                             if (expstkout(i) > specout);
                                foundit = *on;
                             else;
                                i += 1;
                                delay += 1;
                             endif;

                          enddo;

                          pq_delay = delay;

       //caluclate number of days this product is past due
                          if delay = 0;

                             time = 0;
                             exsr $_expected;
                             i = 0;

                             if (exptout >= specout);

                                dou (exptout <= specout) or (i = 5);
                                   i += 1;
                                   time = -1 * i;
                                   exsr $_expected;
                                enddo;

                             endif;

                             delay = (-1 * (i));

       //Enhancement to force totally out of stock products to -5 DELAY.
                             if total_out = '1';
                                delay = -5;
                             endif;

                             pq_delay = delay;

                          endif;

                       endif;

       //-------------------------------------------- calculate min/max days
       //calculate minimum and maximum values
                       minday = pr_sstimef +
                                pq_leadtm  +
                                sp_orcycle;

                       maxday = pr_sstimef +
                                pq_leadtm  +
                                pr_intrval;

       //force calculated maximum days to minimum days
                       if max_to_min = 1;
                          maxday = minday;
                       endif;

       //include add days for regular orders
                       if alt_sour = 0;
                          minday = minday + sp_add_day + pq_add_day;
                       endif;

       //test minday and maxday for large values
                       if  minday > 999;
                           minday = 999;
                       endif;
                       if maxday > 999;
                          maxday = 999;
                       endif;

       //------------------------------------------- calculate min/max units
                       clear @u_use;
                       @u_days = minday;

                       callp K3S_Calc_Usage(@u_comp:
                                            @u_seasonl:
                                            @u_days:
                                            @u_forcast:
                                            @u_forcper:
                                            @u_forcint:
                                            @u_longtrm:
                                            @u_dowmap:
                                            @u_dowper1:
                                            @u_dowper2:
                                            @u_dowper3:
                                            @u_dowper4:
                                            @u_dowper5:
                                            @u_dowper6:
                                            @u_dowper7:
                                            @u_sstper1:
                                            @u_sstper2:
                                            @u_sstper3:
                                            @u_sstper4:
                                            @u_sstper5:
                                            @u_sstper6:
                                            @u_sstper7:
                                            @u_diff12:
                                            @u_diff13:
                                            @u_diff52:
                                            @u_use:
                                            @u_Ds_flag:
                                            @u_DayNbr:
                                            @u_ix:
                                            @u_Ds_days:
                                            @u_Ds_wdis:
                                            @u_Ds_dlyu:
                                            @u_Ds_spct:
                                            @u_Ds_susg:
                                            @u_Ds_accu:
                                            @u_Ds_prno:
                                            @u_Ds_fctr:
                                            @u_Ds_warn);
       //              exsr $_usage;

                       if @u_use <= 9999999;
                          eval(h)   minunit = @u_use;
                       else;
                          minunit = 9999999;
                       endif;

       //   if usage exists, then unit must be at least 1
                       if (minunit = 0) and (@u_use > 0);
                          minunit = 1;
                       endif;

       //   if order up to level <= order point in days, then make both
       //   days and units equal to minimum days and units
                       if (maxday <= minday);
                          maxday = minday;
                          maxunit = minunit;

       //   calculate order up to level in units
                       else;

                          clear @u_use;
                          @u_days = maxday;

                          callp K3S_Calc_Usage(@u_comp:
                                               @u_seasonl:
                                               @u_days:
                                               @u_forcast:
                                               @u_forcper:
                                               @u_forcint:
                                               @u_longtrm:
                                               @u_dowmap:
                                               @u_dowper1:
                                               @u_dowper2:
                                               @u_dowper3:
                                               @u_dowper4:
                                               @u_dowper5:
                                               @u_dowper6:
                                               @u_dowper7:
                                               @u_sstper1:
                                               @u_sstper2:
                                               @u_sstper3:
                                               @u_sstper4:
                                               @u_sstper5:
                                               @u_sstper6:
                                               @u_sstper7:
                                               @u_diff12:
                                               @u_diff13:
                                               @u_diff52:
                                               @u_use:
                                               @u_Ds_flag:
                                               @u_DayNbr:
                                               @u_ix:
                                               @u_Ds_days:
                                               @u_Ds_wdis:
                                               @u_Ds_dlyu:
                                               @u_Ds_spct:
                                               @u_Ds_susg:
                                               @u_Ds_accu:
                                               @u_Ds_prno:
                                               @u_Ds_fctr:
                                               @u_Ds_warn);
       //                 exsr $_usage;

                          eval(h) maxunit = @u_use;

       //   if usage exists, then unit must be at least 1
                          if (maxunit = 0) and (@u_use > 0);
                             maxunit = 1;
                          endif;

                       endif;

       //set min and max days into work fields
                       eval(h) mnwork = minday;
                       eval(h) mxwork = maxday;

       //----------------------- logic for manual minimum and maximum values

       //  only execute if a manual minimum or maximum exists, AND
       //       product has a forecast
                       if (((pr_maminiu > 0) or (pr_maminid > 0) or
                            (pr_mamaxiu > 0) or (pr_mamaxid > 0))
                             AND       pr_forcast > 0)
                                  OR
       //  only execute if a manual minimum or maximum exists, AND
       //       product does not have a forecast, but we allow it
                          (((pr_maminiu > 0) or (pr_maminid > 0) or
                            (pr_mamaxiu > 0) or (pr_mamaxid > 0))
                             AND       pr_forcast = 0
                             AND       average_0  = 1);

                          @m_seasonl = pr_seasonl;
                          @m_forcast = pr_forcast;
                          @m_forcper = pr_forcper;
                          @m_forcint = pr_forcint;
                          @m_longtrm = pr_longtrm;

                          @m_altsour = alt_sour;
                          @m_maminiu = pr_maminiu;
                          @m_mamaxiu = pr_mamaxiu;
                          @m_maminid = pr_maminid;
                          @m_mamaxid = pr_mamaxid;
                          @m_qtybaln = pr_qtybaln;
                          @m_chkopnt = pq_chkopnt;
                          @m_sochk   = so_chkopnt;

                          @m_minday  = minday;
                          @m_maxday  = maxday;
                          @m_minunit = minunit;
                          @m_maxunit = maxunit;
                          @m_mnwork  = mnwork;
                          @m_mxwork  = mxwork;

       //call subroutine to calculate manual minimum or maximum values
                          If ManMinMaxR = 0;
       //     sequence will be Units 1st, Days 2nd
                             callp K3S_Calc_Manual(@m_altsour:
                                                   @m_maminid:
                                                   @m_mamaxid:
                                                   @m_maminiu:
                                                   @m_mamaxiu:
                                                   @m_qtybaln:
                                                   @m_chkopnt:
                                                   @m_sochk:
                                                   @m_seasonl:
                                                   @m_forcast:
                                                   @m_forcper:
                                                   @m_forcint:
                                                   @m_longtrm:
                                                   @m_minday:
                                                   @m_maxday:
                                                   @m_minunit:
                                                   @m_maxunit:
                                                   @m_mnwork:
                                                   @m_mxwork:
                                                   @m_str_day:
                                                   @m_str_unt:
                                                   @u_comp:
                                                   @u_days:
                                                   @u_dowmap:
                                                   @u_dowper1:
                                                   @u_dowper2:
                                                   @u_dowper3:
                                                   @u_dowper4:
                                                   @u_dowper5:
                                                   @u_dowper6:
                                                   @u_dowper7:
                                                   @u_sstper1:
                                                   @u_sstper2:
                                                   @u_sstper3:
                                                   @u_sstper4:
                                                   @u_sstper5:
                                                   @u_sstper6:
                                                   @u_sstper7:
                                                   @u_diff12:
                                                   @u_diff13:
                                                   @u_diff52:
                                                   @u_use:
                                                   @u_Ds_flag:
                                                   @u_DayNbr:
                                                   @u_ix:
                                                   @u_DS_Days:
                                                   @u_DS_wdis:
                                                   @u_DS_dlyu:
                                                   @u_DS_spct:
                                                   @u_DS_susg:
                                                   @u_DS_accu:
                                                   @u_DS_prno:
                                                   @u_DS_fctr:
                                                   @u_DS_warn);
       //                 exsr $_manual;
                          else;
       //                    sequence will be Units 1st, Days 2nd
                             callp K3S_Calc_Manual_R(@m_altsour:
                                                     @m_maminid:
                                                     @m_mamaxid:
                                                     @m_maminiu:
                                                     @m_mamaxiu:
                                                     @m_qtybaln:
                                                     @m_chkopnt:
                                                     @m_sochk:
                                                     @m_seasonl:
                                                     @m_forcast:
                                                     @m_forcper:
                                                     @m_forcint:
                                                     @m_longtrm:
                                                     @m_minday:
                                                     @m_maxday:
                                                     @m_minunit:
                                                     @m_maxunit:
                                                     @m_mnwork:
                                                     @m_mxwork:
                                                     @m_str_day:
                                                     @m_str_unt:
                                                     @u_comp:
                                                     @u_days:
                                                     @u_dowmap:
                                                     @u_dowper1:
                                                     @u_dowper2:
                                                     @u_dowper3:
                                                     @u_dowper4:
                                                     @u_dowper5:
                                                     @u_dowper6:
                                                     @u_dowper7:
                                                     @u_sstper1:
                                                     @u_sstper2:
                                                     @u_sstper3:
                                                     @u_sstper4:
                                                     @u_sstper5:
                                                     @u_sstper6:
                                                     @u_sstper7:
                                                     @u_diff12:
                                                     @u_diff13:
                                                     @u_diff52:
                                                     @u_use:
                                                     @u_Ds_flag:
                                                     @u_DayNbr:
                                                     @u_ix:
                                                     @u_DS_Days:
                                                     @u_DS_wdis:
                                                     @u_DS_dlyu:
                                                     @u_DS_spct:
                                                     @u_DS_susg:
                                                     @u_DS_accu:
                                                     @u_DS_prno:
                                                     @u_DS_fctr:
                                                     @u_DS_warn);
                          endif;

                          minday     = @m_minday;
                          maxday     = @m_maxday;
                          minunit    = @m_minunit;
                          maxunit    = @m_maxunit;
                          mnwork     = @m_mnwork;
                          mxwork     = @m_mxwork;
                          pq_chkopnt = @m_chkopnt;
                          so_chkopnt = @m_sochk;

                       endif;

       //-------------------------------------------------------------------
       //If maximum units less than minimum units, make equal
                       if maxunit < minunit;
                          maxunit = minunit;
                          mxwork  = mnwork;
                       endif;

       //save data into fields for later update
                       pq_opointu = minunit;
                       pq_oruptou = maxunit;

                       pq_opointd = mnwork;

       //filter maximum order up to days to 999
                       pq_oruptox = mxwork;
                       if mxwork > 999;
                          mxtest = %editc(mxwork:'X');
                          pq_oruptod = %dec(%subst(mxtest:5:3):3:0);
                       else;
                          pq_oruptod = mxwork;
                       endif;

                       pq_intrval = pr_intrval;
                       pq_sstimef = pr_sstimef;

       //    save safety stock days into a 3.0 field for screen presentation
                       eval(h)   pq_sstfday = pr_sstimef;

       //car count logic
                       if CarLogic   = 1 and
                          pr_carcoun = 1 and
                          pr_maminiu > 1 and
                          pq_oruptou > pr_maminiu;

                          CarCount   = %div(pq_oruptou:pr_maminiu);
                          CarRemain  = %rem(pq_oruptou:pr_maminiu);
                          if CarRemain  > 0;
                          CarCount   += 1;
                          endif;
                          pq_opointu = CarCount * pr_maminiu;
                          pq_oruptou = CarCount * pr_maminiu;
                       endif;

       //------------------------------------------ calculate order quantity

       //Calculate order quantity if balance falls before order point
                       if pq_qtybaln < pq_opointu;

       //call subroutine to calculate suggested order quantity
                          clear pq_soqact;

                          @q_maxunit = pq_oruptou;
                          @q_minunit = pq_opointu;
                          @q_qtybaln = pq_qtybaln;
                          @q_minqty  = pr_minqty;
                          @q_buymult = pr_buymult;
                          @q_convpak = pr_convpak;
                          @q_convpkp = pr_convpkp;


                          callp K3S_Calc_SOQ(@q_maxunit:
                                             @q_minunit:
                                             @q_qtybaln:
                                             @q_minqty:
                                             @q_buymult:
                                             @q_convpak:
                                             @q_convpkp:
                                             @q_soq);
       //                 exsr $_calc_SOQ;

                          pq_soqact = @q_soq;

       // <<CORE-MARK>>
       //Special logic used for Core-Mark in TRACS
       //    This section is only used if we're at Core-Mark for TRACS,
       //    and this specific chain is set to use the logic.
                          if qc1_on = 1 AND qc1_chain = 1;
                             qc1_expect = ((pr_forcast * pr_forcint)/
                                            365) * sp_orcycle;
                             if qc1_expect > .5 AND qc1_expect < 1.5;
                                qc1_exprnd = qc1_expect + 1;
                                if qc1_exprnd = pq_qtyohnd + pq_soqact;
                                   pq_soqact  = pq_soqact + 1;
                                   pq_soqovrd = 1;
                                endif;
                             endif;
                          endif;

       //Last OP Check test(and Service Level Check where appropriate)
       //this test will capture odd situations where we do end up
       //    needing to order, but the mathmetics up stream for calculating
       //    SPECOUT just would not work. could be caused by slow mover,
       //    that is inexpensive, and has huge buy mult, causing us to
       //    purchase more than a year's worth. we will look at order
       //    time factor of 364 days.
       //after further review... just doesn't make sense
       //    service check (order point check for top items)

                       endif;

               endsl;

       //------------------------------------------------------ all products

       //-------------------------------------------------- Must receive 1st
       //soq exists for product, BUT
       //  customer does not want products suggested prior to receiving
       //  unless nothing has yet been ordered before
               if pq_soqact  > 0 and
                  must_recv  = 1 and
                  pr_lstrcvd = not_recvd and
                  pr_qtyoord > 0;

                  clear pq_soqact;
               endif;

       //-------------------------------------------------- Shut Off test
       //soq exiata for product, BUT  (not for Combined suppliers)
       //  customer using Shut Off function, test this product for
       //  seasonality, and if manual minimum units > 0, and other
       //  tests against dates occur, then set SOQ to 0, and build days too
               if shut_off   = 1 and
                  pr_altsrce = 0 and
                  pr_seasonl <> *blanks and
                  pr_usrstat <> 'M'     and
                  pr_sysstat <> 'D'     and
                  pr_maminiu > 0;

                  WrkNextPer = pr_forcper + 1;
                  if WrkNextPer > pr_forcint;
                     WrkNextPer = 1;
                  endif;

                  if (@u_ix(pr_forcper) = 0 or
                     @u_ix(WrkNextper) = 0);

                     date_outX  = %char(pr_lastupd:*iso);
                     date_outA = %replace('-01':date_outX:8:3);
                     date_out = %date(date_outA:*iso);
                     date_out += %months(1);
                     date_out -= %days(days_out);

       // this period and next period have 0 factors
                     if (@u_ix(pr_forcper) = 0 and
                        @u_ix(WrkNextper) = 0);

                        clear pq_soqact;
                        clear pq_opointu;
                        clear pq_oruptou;
                        pq_nonstck = 1;
                     endif;

       // this period 0 factor but entering the season
                     if (@u_ix(pr_forcper) = 0 and
                        pr_lastupd < date_out);
                        clear pq_soqact;
                        clear pq_opointu;
                        clear pq_oruptou;
                        pq_nonstck = 1;
                     endif;

       // next period 0 factor and we are close to ending season
                     if (@u_ix(WrkNextPer) = 0 and
                        pr_lastupd >= date_out);
                        clear pq_soqact;
                        clear pq_opointu;
                        clear pq_oruptou;
                        pq_nonstck = 1;
                     endif;

                  endif;
               endif;

       //---------------------------------------------- Exclude buying until

       //soq exists for product, BUT
       //   we want to Exclude Buying Until a specific date in the future,
       //   (for auto parts, as long as no back orders exist)
               if pq_soqact > 0;
                  if pr_excuntl > lc_sysdate;
                     if excl_buy = 1;
                        if pr_qtyback = 0;
                           clear pq_soqact;
                        endif;
                     else;
                        clear pq_soqact;
                     endif;
                  endif;
               endif;

       //---------------------------------------------------- Product checks

       //soq exists for product, and this is a regular order
       //                       and we are not trying to keep quantity
       //                         The night job sets keepqty = '0'
       //                         During the day, the buyer has choice
       //                           to set keepqty to '0' or '1'
               if pq_soqact > 0 and alt_sour = 0
                                and keepqty  = '0';
                  pq_chksoq = 1;
                  so_chksoq += 1;
               endif;

       //forecast frozen check
               if pr_forfrez > lc_sysdate;
                  pq_chkfrez = 1;
                  so_chkfrez += 1;
               endif;

       //buying more than six months check
               if pq_soqact > (pr_forcast * pr_forcint * .5);
                  pq_chk6mon = 1;
                  so_chk6mon += 1;
               endif;

       //back order for product, and suggested order exists
               if (pq_qtyback > 0) and (pq_soqact > 0);
                  pq_chkback = 1;
                  so_chkback += 1;
               endif;

       //watch products accumulation
               if pr_usrstat = 'W';
                  pq_chkwatc = 1;
                  so_chkwatc += 1;
               endif;

       //probation products accumulation
               if pr_usrstat = 'P';
                  if pr_probdat >= lc_sysdate;
                     if check_pr = 1;
                        diff_days = %diff(pr_probdat:lc_sysdate:*days);
                        if diff_days  <= 10;
                           pq_chkprob = 1;
                           so_chkprob += 1;
                        endif;
                     else;
                        pq_chkprob = 1;
                        so_chkprob += 1;
                     endif;
                  endif;
               endif;

       //contract exists accumulation
               if pr_contflg = 1;
                  pq_chkcont = 1;
                  so_chkcont += 1;
               endif;

       //overstocked check
               if (pq_soqact > 0) and (pq_overflg = 1)
                                  and (pq_overunt = 0);
                  pq_chkover = 1;
                  so_chkover += 1;
               endif;

       //PE checks PE1 thru PE5 only (not div erter orders !)
               if (pr_altsrce <> 1) and
                  ((pr_endper = '1') or
                  (pr_endper = '2') or
                  (pr_endper = '3') or
                  (pr_endper = '4') or
                  (pr_endper = '5'));

                  select;
                     when pr_endper = '1';
                          reqtype_pd = 'PE1';
                     when pr_endper = '2';
                          reqtype_pd = 'PE2';
                     when pr_endper = '3';
                          reqtype_pd = 'PE3';
                     when pr_endper = '4';
                          reqtype_pd = 'PE4';
                     when pr_endper = '5';
                          reqtype_pd = 'PE5';
                  endsl;

                  select;
                     when pr_forcint = 52;
                          ending_dt  = ending_52;
                     when pr_forcint = 12;
                          ending_dt  = ending_12;
                     when pr_forcint = 13;
                          ending_dt  = ending_13;
                  endsl;
                  ending_SET = ending_dt + %days(7);

                  if pr_altsrce = 2;
                     xx_locn    = pr_cmblocn;
                     xx_supl    = pr_cmbsupl;
                     xx_suplsub = pr_cmbsub;
                  else;
                     xx_locn    = pr_locn;
                     xx_supl    = pr_supl;
                     xx_suplsub = pr_suplsub;
                  endif;

                  exsr InzInpSrchpd;

                  exsr IntSQLStmtpd;

                  exsr PrepDynSQLStmtpd;

                  if SQLState = SQLStateOk;

                     exsr opnpdcursor;

                     if SQLState = SQLStateOk;

                        exec sql
                          fetch next
                            from pdcusror
                            into :prodsed_rec;

                        if SQLState = SQLStateOk;

                           if pd_birth  >= ending_dt  and
                              pd_review  = 0;

                              pq_chkchg = 1;
                              so_chkchg += 1;
                           endif;
                        endif;
                     endif;
                     exsr clspdcursor;
                  endif;
               endif;

       //accumulate product totals
               pq_chkrevu = 1;
               so_chkrevu += 1;

       //All Frozen with forecast = 0 testing
               if all_frozen = 1   and
                  pr_altsrce <> 1  and
                  pr_usrstat = 'F' and
                  pr_forcast =  0;
                  all_froz_0 += 1;
               endif;

       //order point check and service point check must have SOQ
               if pq_soqact = 0;
                  if pq_chkopnt = 1;
                     pq_chkopnt = 0;
                     so_chkopnt -= 1;
                  endif;
                  if pq_chkserv = 1;
                     pq_chkserv = 0;
                     so_chkserv -= 1;
                  endif;
               endif;

       //product reminders check
       //-- start product reminders logic
               if alt_sour = 0 AND check_RE = 1;

       // there are four combinations that can occur for product reminders
       // as soon as a Type condition occurs, the remaining Types are not
       // checked
       //                  Location   Supplier
       //    Type 1 -        Yes       Yes
       //    Type 2 -        blank     Yes
       //    Type 3 -        Yes       blank
       //    Type 4 -        blank     blank

       // Type 1 Start ------------------------
                  typval_nt = 'P';
                  deal_nt = *blanks;
                  seasonl_nt = *blanks;
                  prod_nt = pr_prod;

       //     generate key for testing
       //        combined supplier
                 if sp_altsrce = 2;
                    locn_nt    = pr_cmblocn;
                    supl_nt    = pr_cmbsupl;
                    suplsub_nt = pr_cmbsub;

       //        regular supplier
                 else;
                    locn_nt    = pr_locn;
                    supl_nt    = pr_supl;
                    suplsub_nt = pr_suplsub;
                 endif;

       //     first test, does a reminder record exist?

       //     if yes, then see if we have reached reminder date,
       //             and that expire date in future
                 exec sql
                   select count(*)
                     into :notepad_count
                     from k_notepad
                     where nt_comp = :pr_comp and
                           nt_typval = :typval_nt and
                           nt_locn = :locn_nt and
                           nt_supl = :supl_nt and
                           nt_suplsub = :suplsub_nt and
                           nt_prod = :prod_nt and
                           nt_deal = :deal_nt and
                           nt_seasonl = :seasonl_nt and
                           nt_typnote = :typnote_RE;

                 if notepad_count > 0;

                    exsr InzInpSrchnt;
    ‚   //initialize StmtString
                    exsr IntSQLStmtnt;

    ‚                exsr PrepDynSQLStmtnt;

                    if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                       exsr opnntcursor;

                       dow SQLState = SQLStateOk;

                           exec sql
                             fetch next
                                from ntcursor
                                into :notepad_rec;

                           if SQLState = RowNotFound;
                              leave;
                           endif;

       //     if we have reached reminder date, get out
                           if nt_remind <=  lc_sysdate;
                              if (nt_expire > first_day AND
                                 nt_expire > lc_sysdate) OR
                                 (nt_expire = first_day);

                                 pq_chkremi = 1;
                                 so_chkremi += 1;
                                 leave;
                              endif;
                           endif;
                       enddo;
                       exsr clsntcursor;
                    endif;
                 endif;
       //      *in22 test
       // Type 1 End   ------------------------

       // Type 2 Start ------------------------

       //    only continue if Type 1 was not an RE check
                 if pq_chkremi = 0;

                    typval_nt = 'P';
                    deal_nt = *blanks;
                    seasonl_nt = *blanks;

                    prod_nt = pr_prod;

       //     generate key for testing
       //        combined supplier
                    if sp_altsrce = 2;
                       locn_nt    = *blanks;
                       supl_nt    = pr_cmbsupl;
                       suplsub_nt = pr_cmbsub;

       //        regular supplier
                    else;
                       locn_nt    = *blanks;
                       supl_nt    = pr_supl;
                       suplsub_nt = pr_suplsub;
                    endif;

       //     first test, does a reminder record exist?
       //     if yes, then see if we have reached reminder date,
       //             and that expire date in future
                    exec sql
                      select count(*)
                        into :notepad_count
                        from k_notepad
                        where nt_comp = :pr_comp and
                              nt_typval = :typval_nt and
                              nt_locn = :locn_nt and
                              nt_supl = :supl_nt and
                              nt_suplsub = :suplsub_nt and
                              nt_prod = :prod_nt and
                              nt_deal = :deal_nt and
                              nt_seasonl = :seasonl_nt and
                              nt_typnote = :typnote_RE;

                    if notepad_count > 0;

                       exsr InzInpSrchnt;
    ‚   //initialize StmtString
                       exsr IntSQLStmtnt;

    ‚                 exsr PrepDynSQLStmtnt;

                       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                          exsr opnntcursor;

                          dow SQLState = SQLStateOk;

                              exec sql
                                fetch next
                                   from ntcursor
                                   into :notepad_rec;

                              if SQLState = RowNotFound;
                                 leave;
                              endif;

       //     if we have reached reminder date, get out
                              if nt_remind <=  lc_sysdate;

                                 if (nt_expire > first_day AND
                                    nt_expire > lc_sysdate) OR
                                    (nt_expire = first_day);
                                       pq_chkremi = 1;
                                       so_chkremi += 1;
                                       leave;
                                 endif;
                              endif;
                          enddo;
                          exsr clsntcursor;
                       endif;
                    endif;
       //      *in22 test
                 endif;
       //      pq_chkremi = 0
       // Type 2 End   ------------------------

       // Type 3 Start ------------------------

       //    only continue if Type 2 was not an RE check
                 if pq_chkremi = 0;

                    typval_nt = 'P';
                    deal_nt = *blanks;
                    seasonl_nt = *blanks;
                    prod_nt = pr_prod;

       //     generate key for testing
       //        combined supplier
                    if sp_altsrce = 2;
                       locn_nt    = pr_cmblocn;
                       supl_nt    = *blanks;
                       suplsub_nt = *blanks;

       //        regular supplier
                    else;
                       locn_nt    = pr_locn;
                       supl_nt    = *blanks;
                       suplsub_nt = *blanks;
                    endif;

       //     first test, does a reminder record exist?
                    exec sql
                      select count(*)
                        into :notepad_count
                        from k_notepad
                        where nt_comp = :pr_comp and
                              nt_typval = :typval_nt and
                              nt_locn = :locn_nt and
                              nt_supl = :supl_nt and
                              nt_suplsub = :suplsub_nt and
                              nt_prod = :prod_nt and
                              nt_deal = :deal_nt and
                              nt_seasonl = :seasonl_nt and
                              nt_typnote = :typnote_RE;

                    if notepad_count > 0;

                       exsr InzInpSrchnt;
    ‚   //initialize StmtString
                       exsr IntSQLStmtnt;

    ‚                 exsr PrepDynSQLStmtnt;

                       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                          exsr opnntcursor;

                          dow SQLState = SQLStateOk;

                              exec sql
                                fetch next
                                   from ntcursor
                                   into :notepad_rec;

                              if SQLState = RowNotFound;
                                 leave;
                              endif;

       //     if we have reached reminder date, get out
                              if nt_remind <=  lc_sysdate;

                                 if (nt_expire > first_day AND
                                    nt_expire > lc_sysdate) OR
                                    (nt_expire = first_day);

                                    pq_chkremi = 1;
                                    so_chkremi += 1;
                                    leave;
                                 endif;
                              endif;
                          enddo;
                          exsr clsntcursor;
                    endif;
       //      *in22 test
                 endif;
       //      pq_chkremi = 0
       // Type 3 End   ------------------------

       // Type 4 Start ------------------------

       //    only continue if Type 3 was not an RE check
                 if pq_chkremi = 0;

                    typval_nt = 'P';
                    deal_nt = *blanks;
                    seasonl_nt = *blanks;
                    locn_nt   = *blanks;
                    supl_nt    = *blanks;
                    suplsub_nt = *blanks;
                    prod_nt = pr_prod;

       //     first test, does a reminder record exist?
       //     if yes, then see if we have reached reminder date,
       //             and that expire date in future
                    exec sql
                      select count(*)
                        into :notepad_count
                        from k_notepad
                        where nt_comp = :pr_comp and
                              nt_typval = :typval_nt and
                              nt_locn = :locn_nt and
                              nt_supl = :supl_nt and
                              nt_suplsub = :suplsub_nt and
                              nt_prod = :prod_nt and
                              nt_deal = :deal_nt and
                              nt_seasonl = :seasonl_nt and
                              nt_typnote = :typnote_RE;

                    if notepad_count > 0;

                       exsr InzInpSrchnt;
    ‚   //initialize StmtString
                       exsr IntSQLStmtnt;

    ‚                  exsr PrepDynSQLStmtnt;

                       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                          exsr opnntcursor;

                          dow SQLState = SQLStateOk;

                              exec sql
                                fetch next
                                   from ntcursor
                                   into :notepad_rec;

                              if SQLState = RowNotFound;
                                 leave;
                              endif;

       //     if we have reached reminder date, get out
                              if nt_remind <=  lc_sysdate;
                                 if (nt_expire > first_day AND
                                    nt_expire > lc_sysdate) OR
                                    (nt_expire = first_day);

                                      pq_chkremi = 1;
                                      so_chkremi += 1;
                                 endif;
                              endif;
                          enddo;
                          exsr clsntcursor;
                       endif;
                    endif;
       //      *in22 test
                 endif;
       //      pq_chkremi = 0
       // Type 4 End   ------------------------

               endif;
       // --- end   product reminders logic

       //-------------------------------------------------------------- Deals

       //   should deal system be used to calculate extra days?
               if (use_deals = '1') and
                  (alt_sour = 0)  and
                  (pr_costeac > 0);

       //process deal information

       //   A deal does exist for this product,
       //      and it has not passed today's date. This is deal check.
                  if (pr_deal    <> *blanks) and
                     (pr_dealend >= lc_sysdate);

                     pq_chkdeal = 1;
                     so_chkdeal += 1;

       //   has deal started? if so continue to see if calculations needed
                     if pr_dealbeg <= lc_sysdate;

       //   If deal is in 5 day window, or a limited buy deals exists,
       //   then calculate extra days. Also, move product to forward buy
       //   check, and out of deal check.
                        if (pr_dealalw = 0 and
                           pr_dealend <= dealwindow)   OR
                           (pr_dealalw > 0 and
                           pr_dealalw > pr_dealuse);

                           pq_chkdeal = 0;
                           so_chkdeal -= 1;
                           pq_chkfbuy = 1;
                           so_chkfbuy += 1;

       //convert values to be passed from 3.1 to 3.3 fields
                           vldatng = lc_vldatng * .01;

       //----
       // call to module that calculates forward buy extra days
                           mode# = 1;
                           deal_days = 0;
                           disc_accum = 0;
                           off_invce = 0;
                           off_reg = 0;
                           callp K3S_M070 (mode#:
                                           pr_comp:
                                           hold_locn:
                                           pr_deal:
                                           pr_prod:
                                           pr_costeac:
                                           lc_sysdate:
                                           dealwindow:
                                           lc_vldisct:
                                           lc_vlpincr:
                                           vldatng:
                                           deal_days:
                                           disc_accum:
                                           off_invce:
                                           pr_costreg:
                                           off_reg);

       //   Do not buy out for Daily Replenishment environment
                           if vary_orcyc = 1 AND
                              @u_dowmap  = 1;

                              deal_days  = 0;
                           endif;

       //   extra days to buy (back out order cycle and supplier lead time)
                           if pr_invmeth = 0;
                              if (deal_days - sp_orcycle
                                            - sp_leadtmo) < -999;
                                 pq_fbxdays = 0;
                              else;
                                 pq_fbxdays = deal_days - sp_orcycle
                                                    - sp_leadtmo;
                                 if pq_fbxdays < 0;
                                    pq_fbxdays = 0;
                                 endif;
                              endif;
                           endif;
       //   extra days to buy straight
                           if pr_invmeth = 1;
                              if deal_days  < -999;
                                 pq_fbxdays = 0;
                              else;
                                 pq_fbxdays = deal_days;
                                 if pq_fbxdays < 0;
                                    pq_fbxdays = 0;
                                 endif;
                              endif;
                           endif;

       // <<CH/BW>>
       //   Rx=240 days, OTC=90 days.  Rx products coded 'Y' in group1
       //   Special code for Bindley, controlled by 'QQQ' table code record
                           if rx_otc = 1;
                              if pr_group1 = 'Y';
                                 lc_fbmax = 240;
                              else;
                                 lc_fbmax = 90;
                              endif;
                           endif;

       //  check against forward buy maximum days value
                           if pq_fbxdays > pq_maxdays;
                              pq_fbxdays = pq_maxdays;
                           endif;

       //   check against forward buy filter days value
                           if pq_fbxdays > lc_fbfiltr;
                              pq_chkfbfl = 1;
                              so_chkfbfl += 1;
                           endif;
       //----

                        else;

       //  We did not execute K3S_M070 above because we were not in the
       //  deal window, and unlimited buys allowed. However, we are in
       //  the deal period, so we need to call K3S_M070 to calculate the
       //  off invoice amount to subtract from regular cost.

       //convert values to be passed from 3.1 to 3.3 fields
                           vldatng = lc_vldatng * .01;

       //----
       // call to module that calculates forward buy extra days
                           mode# = 2;
                           deal_days = 0;
                           disc_accum = 0;
                           off_invce = 0;
                           off_reg = 0;
                           callp K3S_M070(mode#:
                                          pr_comp:
                                          hold_locn:
                                          pr_deal:
                                          pr_prod:
                                          pr_costeac:
                                          lc_sysdate:
                                          dealwindow:
                                          lc_vldisct:
                                          lc_vlpincr:
                                          vldatng:
                                          deal_days:
                                          disc_accum:
                                          off_invce:
                                          pr_costreg:
                                          off_reg);

       //  in 5 day window OR buys allowed > 0
                        endif;

                        pq_costeac = pq_costeac - off_invce;
                        pq_costord = pr_costreg - (off_invce *
                                                 pq_costdiv);
                        if pq_costdiv > 1 AND pr_costreg > off_reg;
                           pq_costord = pr_costreg - off_reg;
                        endif;

       //  deal has begun
                     endif;

       //  deal exists and ends in future
                  endif;

       //  Permanent Deals for 'off invoice' testing 'P2' type only
       //  (specific logic to handle USA Drug)
                  if perm_deal = 1;

                     exec sql
                       select *
                         into :dealper_rec
                         from k_dealper
                         where dr_comp = :pr_comp and
                               dr_permtyp = 'P2' and
                               dr_locn = :hold_locn and
                               dr_supl = :hold_supl and
                               dr_suplsub = :hold_sub and
                               dr_prod = :pr_prod and
                               dr_type = 'OI'
                         fetch first row only;

                     if SQLState = SQLStateOk and
                        dr_apply   = 'O' and
                        dr_unit    = pq_costdiv and
                        dr_facttyp = '$';

                        pq_costord = dr_factval;
                        pq_costeac = pq_costord / pq_costdiv;
                     endif;

       // (specific logic to handle Resnick Distributors - begin
                     if SQLState = SQLStateOk and
                        dr_apply   = 'O' and
                        dr_factval > 0   and
                        dr_facttyp = '%';

                        pq_costord = pq_costord -
                                     (dr_factval * .01 * pq_costord);
                        pq_costeac = pq_costord / pq_costdiv;
                     endif;
                     if SQLState = SQLStateOk and
                        dr_apply   = 'O' and
                        dr_factval > 0   and
                        dr_facttyp = 'D';

                        pq_costord = pq_costord -
                                     (dr_factval / dr_unit);
                        pq_costeac = pq_costord / pq_costdiv;
                     endif;
       //  (specific logic to handle Resnick Distributors - end

                  endif;

       //  use deals?
               endif;

       //-------------------------------------------------- alternate source

       //calculate extra quantity to purchase for alternate source orders
               if alt_sour = 1;

       //if a replenishment quantity has been generated or if distributor
       //    wants to look at all products
                  if pq_soqact  > 0  OR
                     altsrc_all = 1;

       //   prepare work fields to be passed as parameters
                     cashdsc = so_cashdsc * .01;
                     rebate  = so_rebate  * .01;
                     intrate = lc_intrate * .01;
                     eval return  = lc_return  * .01;

                     invmeth = pr_invmeth;
                     deal_days = 0;
                     restr_qty = 0;
                     last_recd = 0;
       //   call program to process alternate source products
                     callp K3S_M071(so_comp:
                                    so_locn:
                                    pr_prod:
                                    so_supl:
                                    so_suplsub:
                                    cashdsc:             //cash discount
                                    rebate:              //rebate
                                    intrate:            //interest rate
                                    return:             //return on investment
                                    invmeth:            //investment method
                                    deal_days:          //extra days to buy
                                    restr_qty:          //restricted quantity
                                    last_recd);         //last record off

       //if this is a restricted quantity product, then use restricted
       //   quantity
                     if restr_qty > 0;
                        pq_soqact  = restr_qty;
                        pq_soqsrvc = restr_qty;
                        pq_fbxdays = 0;
                        pq_restflg = 1;
       //   accumulate restricted products to user 1 check
                        pq_chkusr1 = 1;
                        so_chkusr1 += 1;

       //this is not a restricted quantity product, so use extra forward
       //   buy days
                     else;

       //   save extra days to buy
                        pq_fbxdays = deal_days;

       //   if no extra days to buy, then set SOQ to 0
                        if pq_fbxdays = 0;
                           pq_soqact  = 0;
                        else;
       //   accumulate non-restricted products to user 2 check
                           pq_chkusr2 = 1;
                           so_chkusr2 += 1;
                        endif;

       //   stop at maximum allowable extra days for alt source orders
                        if pq_fbxdays > lc_altmaxd;
                           pq_fbxdays = lc_altmaxd;
                        endif;

                     endif;

       // PQ_SOQACT > 0 or ALTSRC_ALL = 1 test
                  endif;

       // end alt_sour = 1
               endif;

       //-------------------------------------------------------------------
       //There is a scenario where very slow moving products can have a zero SQ
       //but delay is < 0 (meaning delay would show in red on K3S_1040).
       //Be sure to set delay = zero since suggested quantity = zero.
               if pq_soqact = 0 and
                  pq_delay < 0;

                  pq_delay = 0;
               endif;

       //------------------------------------------------- save original soq
       //save original suggested order quantity
               pq_soqsrvc = pq_soqact;
               pr_soqnite = pq_soqact;

       //------------------------------------------------------ product delay
       //for 'daily replenishment' environment control the product delay
               if vary_orcyc = 1 AND
                  @u_dowmap  = 1;

                  pq_soqeso1 = 1.111;
                  pq_soqeso2 = 2.222;
                  pq_soqeso3 = 3.333;
                  pq_soqeso4 = 4.444;
                  pq_soqeso5 = 5.555;
                  pq_soqeso6 = 6.666;
                  if pq_chksoq  = 1;
                     pq_delay   = 0;
                     pq_soqsso  = 1.000;
                    if pq_qtyohnd = 0;
                       pq_delay   = -1;
                    endif;
                  else;
                     pq_delay   = 1;
                     pq_soqsso  = 0.000;
                  endif;
               endif;

       //------------------------------------------------- accumulate totals

       //accumulate values if suggested order quantity exists

               if pq_soqact > 0;

        //dollars regular
B06€               monitor;
                     so_replreg += (pq_soqact * pr_costeac);
  06                 on-error  103;
  06                 so_replreg = 999999999;
E06€               endmon;
                  if (day_time = 0);
                     so_actureg = so_replreg;
                  endif;

       // dollars net
B06€               monitor;
                     so_replnet += (pq_soqact * pq_costeac);
  06                 on-error  103;
  06                 so_replnet = 999999999;
E06€               endmon;
                  if (day_time = 0);
E06€                 so_actunet = so_replnet;
                  endif;

       // weight
                  if pq_weightd > 0;
B06€                  monitor;
                        eval(h)   so_replwgt += (pq_soqact
                                                * (pq_weight / pq_weightd));
  06                    on-error  103;
  06                    so_replwgt = 999999999;
                     endmon;
  06                 if so_replwgt > 999999999;
                        so_replwgt = 999999999;
                     endif;
                     if (day_time = 0);
                        so_actuwgt = so_replwgt;
                     endif;
                  endif;

       //   volume
                  if pq_volumed > 0;
B06€                  monitor;
                        so_replvol += (pq_soqact * (pq_volume / pq_volumed));
  06                    on-error  103;
  06                    so_replvol = 999999999;
E06                  endmon;
  06                 if so_replvol > 999999999;
  06                    so_replvol = 999999999;
E06€                  endif;
                     if (day_time = 0);
                        so_actuvol = so_replvol;
                  endif;
               endif;

       // purchase increment
               if pq_purincr > 0;
B06€               monitor;
                     so_replpqt += (pq_soqact / pq_purincr);
  06                 on-error  103;
  06                 so_replpqt = 999999999;
E06€               endmon;
                  if (day_time = 0);
                     so_actupqt = so_replpqt;
                  endif;
               endif;

       // other
B06            monitor;
                  so_reploth += (pq_soqact * pq_disothr);

  06              on-error  103;
  06              so_reploth = 999999999;
E06€            endmon;
               if (day_time = 0);
                  so_actuoth = so_reploth;
               endif;

       // unit 7
B06€            monitor;
                  so_replun7 += (pq_soqact * pr_disunt7);
  06              on-error  103;
  06              so_replun7 = 999999999;
E06€            endmon;
               if (day_time = 0);
                  so_actuun7 = so_replun7;
               endif;

       // unit 8
B06€            monitor;
                  so_replun8 += (pq_soqact * pr_disunt8);
  06              on-error  103;
  06              so_replun8 = 999999999;
E06€            endmon;
               if (day_time = 0);
                  so_actuun8 = so_replun8;
               endif;

       // unit 9
B06€            monitor;
                  so_replun9 += (pq_soqact * pr_disunt9);
  06              on-error  103;
  06              so_replun9 = 999999999;
E06€            endmon;
               if (day_time = 0);
                  so_actuun9 = so_replun9;
               endif;

       // save line extension

               if pq_soqact * pq_costeac > 9999999;
                  pq_dolrext = 9999999;
               else;
                  pq_dolrext = pq_soqact * pq_costeac;
               endif;

            endif;

       //replace soq with overridden soq from previous order
            if (day_time = 1) and
               (keepqty = '1') and
               (soqovrdsav = 1);

               pq_soqact = soqactkeep;
               pq_soqovrd = soqovrdsav;
            endif;
       //  if day time, and buyer had decided to keep over-ridden qty's,
       //     then determine SOQ check
            if day_time = 1  AND
               keepqty = '1' AND
               pq_soqact > 0;

               pq_chksoq = 1;
               so_chksoq += 1;
            endif;
       //calculate actunet values for day_time
            if (day_time = 1) and
               (pq_soqact > 0);
       // dollars regular
               monitor;
                  so_actureg += (pq_soqact * pr_costeac);
  06              on-error  103;
  06              so_actureg = 999999999;
E06€            endmon;

       // dollars net
               monitor;
                  so_actunet += (pq_soqact * pq_costeac);
  06              on-error  103;
  06              so_actunet = 999999999;
E06€            endmon;

       // weight
               if pq_weightd > 0;
                  monitor;
                     eval(h) so_actuwgt += (pq_soqact *
                                           (pq_weight / pq_weightd));
  06                 on-error  103;
  06                 so_actuwgt = 999999999;
E06€               endmon;
  06              if so_actuwgt > 999999999;
  06                 so_actuwgt = 999999999;
E06€               endif;
               endif;

       // volume
               if pq_volumed > 0;
                  monitor;
                     so_actuvol += (pq_soqact * (pq_volume / pq_volumed));
  06                 on-error  103;
  06                 so_actuvol = 999999999;
E06€               endmon;
  06              if so_actuvol > 999999999;
  06                 so_actuvol = 999999999;
E06€               endif;
               endif;

       // purchase increment
               if pq_purincr > 0;
                  monitor;
                     so_actupqt += (pq_soqact / pq_purincr);
  06                 on-error  103;
  06                 so_actupqt = 999999999;
E06€               endmon;
               endif;

       // other
               monitor;
                  so_actuoth += (pq_soqact * pq_disothr);
  06              on-error  103;
  06              so_actuoth = 999999999;
E06€            endmon;

       // unit 7
               monitor;
                  so_actuun7 += (pq_soqact * pr_disunt7);
  06              on-error  103;
  06              so_actuun7 = 999999999;
E06€            endmon;

       // unit 8
               monitor;
                  so_actuun8 += (pq_soqact * pr_disunt8);
  06              on-error  103;
  06              so_actuun8 = 999999999;
E06€            endmon;

       // unit 9
               monitor;
                  so_actuun9 += (pq_soqact * pr_disunt9);
  06              on-error  103;
  06              so_actuun9 = 999999999;
E06€            endmon;

       //  save line extension

               if pq_soqact * pq_costeac > 9999999;
                  pq_dolrext = 9999999;
               else;
                  pq_dolrext = pq_soqact * pq_costeac;
               endif;

            endif;

       //---------------------------------------------- update product record

       // update product record
            exsr update_product;

       // write  product suggested order record
       //        save suggested order sequence #
            pq_soqseq# = sp_soqseq#;
            soqseq# = sp_soqseq#;

            exsr insert_prodsoq;

       //only process products with valid date end
            endif;

         endif;
       //determine if product is active during the nite job end

         endif;
       //read products end of loop
         SQLState = SQLStateOk;
         enddo;
         exsr clsprcursor;
         endif;

       //--------------------------------------------------- End of Main Loop

       //     update local data area *lda only if it is night time
       if day_time = 0;
          out *dtaara;
       endif;

       //write supplier soq record for last supplier
       exsr $_writesoq;

       //finished, set on LR
       *inlr = *on;

       //for alternate source orders, at LR time, shut down K3S_M071,K3S_M150
       if alt_sour = 1 AND *inlr = *on;
       //shut down
          last_recd = 1;
          callp K3S_M071(so_comp:
                         so_locn:
                         pr_prod:
                         so_supl:
                         so_suplsub:
                         cashdsc:                   //cash discount
                         rebate:                    //rebate
                         intrate:                   //interest rate
                         return:                    //return on investment
                         invmeth:                   //investment method
                         deal_days:                 //extra days to buy
                         restr_qty:                 //restricted quantity
                         last_recd);                //last record off
       //   shut down
          callp K3S_M150(pq_comp:
                         pq_locn:
                         pq_suplorg:
                         pq_suplors:
                         supl_name:
                         last_recd);                //last record off
       endif;

       //**************************************************** End of program

       //////////////////////////////////////////////////////// Get Supplier

       begsr $_getsupl;

       #comp = pr_comp;
       #buyr = pr_buyr;
       #locn = pr_locn;
       #supl = pr_supl;
       #suplsub = pr_suplsub;

       so_comp = pr_comp;
       so_buyr = pr_buyr;
       so_locn = pr_locn;
       so_supl = pr_supl;
       so_suplsub = pr_suplsub;

       //get supplier record
       exec sql
         select *
           into :suplier_rec
           from k_suplier
           where sp_comp = :pr_comp and
                 sp_locn = :pr_locn and
                 sp_supl = :pr_supl and
                 sp_suplsub = :pr_suplsub
           fetch first row only;

       if SQLState = SQLStateOk;

       //save supplier order cycle if alternate source order
          if alt_sour = 1;
             savorcycle = sp_orcycle;
          endif;

       //save last suggested order sequence #
          savsoqseq# = sp_soqseq#;

       //increment suggested order sequence #
       //daytime increment by 1
          if day_time = 1;
             sp_soqseq# += 1;
          endif;
       //nite time (or alternate source) reset sequence # to 1
          if day_time = 0 OR alt_sour = 1;
             sp_soqseq# = 1;
          endif;

       //update supplier record
          exec sql
            update k_suplier
              set sp_soqseq# = :sp_soqseq#
              where sp_comp = :pr_comp and
                    sp_locn = :pr_locn and
                    sp_supl = :pr_supl and
                    sp_suplsub = :pr_suplsub;

       endif;

       //-------  variable order cycle section -- begin
       //check to see if supplier has variable order cycle record
       vary_orcyc = 0;
       @u_dowmap  = 0;

       exec sql
         select *
           into :suplvoc_rec
           from k_suplvoc
           where si_comp = :pr_comp and
                 si_locn = :pr_locn and
                 si_supl = :pr_supl and
                 si_suplsub = :pr_suplsub
           fetch first row only;
       if SQLState = SQLStateOk;
          vary_orcyc = 1;
          save_orcyc = sp_orcycle;
       endif;

       //variable order cycle does exist, determine correct value
       if vary_orcyc = 1;
       //  D=Daily
          if si_rectype = 'D';
             select;
                when DayNbr = 1;
                     sp_orcycle = si_orcyc01;       //order cycle
                when DayNbr = 2;
                     sp_orcycle = si_orcyc02;
                when DayNbr = 3;
                     sp_orcycle = si_orcyc03;
                when DayNbr = 4;
                     sp_orcycle = si_orcyc04;
                when DayNbr = 5;
                     sp_orcycle = si_orcyc05;
                when DayNbr = 6;
                     sp_orcycle = si_orcyc06;
                when DayNbr = 7;
                     sp_orcycle = si_orcyc07;
             endsl;
       //    SI_ORCYCxx = 0 means this is NOT an order day
             Not_Today  = 0;
             if sp_orcycle = 0;
                Not_Today  = 1;
             endif;
       //   just in case bad data gets into K_SUPLVOC field
             if sp_orcycle < 1;
                sp_orcycle = 1;
             endif;
       //    save off Weekly Distribution values
             @u_Ds_flag = 0;
             @u_DayNbr  = DayNbr;
             @u_dowmap  = si_dowmap;                 //day of week map flag
          endif;

       endif;
       //-------  variable order cycle section -- end

       endsr;

       /////////////////////////////////////////// Write Supplier SOQ Record

       begsr $_writesoq;

       exec sql
         select *
           into :suplsoq_rec
           from k_suplsoq
           where so_comp = :#comp and
                 so_buyr = :#buyr and
                 so_locn = :#locn and
                 so_supl = :#supl and
                 so_suplsub = :#suplsub and
                 so_soqseq# = :sp_soqseq#
           fetch first row only;

       soqseq# = sp_soqseq#;
       if SQLState = RowNotFound;   //row not found

       // so_comp    = pr_comp;
       // so_locn    = pr_locn;
          so_soqseq# = sp_soqseq#;
       // so_buyr    = sp_buyr;
       // so_supl    = sp_supl;
       // so_suplsub = sp_suplsub;
          so_name    = sp_name;
          so_dispseq = sp_dispseq;

       // Group suggested orders by User ID?
          if user_group = 1;

             exec sql
               select ta_codeds1
                 into :tacodeds1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'BUY' and
                       ta_codeval = :so_buyr
                 fetch first row only;
               if SQLState = SQLStateOk;
                  so_dispseq = tacodeds1;
               endif;
          endif;

          so_soqseq# = sp_soqseq#;
          so_brkothr = sp_brkothr;
          so_dsptot1 = sp_dsptot1;
          so_dsptot2 = sp_dsptot2;
          so_lcktime = default_time;
          so_birth   = lc_sysdate;
          so_altsrce = sp_altsrce;
          so_ordate  = sp_ordate;
          so_rvdate  = lc_sysdate;
          so_expdelv = lc_sysdate + %days(sp_leadtmq);
          so_fxcfrq  = sp_fxcfrq;
          so_fxcday  = sp_fxcday;
          so_fxclst  = sp_fxclst;
          so_fxcnxt  = sp_fxcnxt;
          so_headcst = sp_headcst;
          so_leadtmq = sp_leadtmq;
          so_leadtmo = sp_leadtmo;
          so_leadtmv = sp_leadtmv;
          so_service = sp_service;
          so_maxdays = sp_maxdays;
          so_add_day = sp_add_day;
          so_precise = sp_precise;
          so_autopo  = sp_autopo;
          so_potype  = sp_potype;
          so_rebate  = sp_rebate;
          so_cashdsc = sp_cashdsc;
       // so_prefseq = sp_prefseq;

       //field SO_PREFSEQ not used anywhere, was available for us to signal
       //  to program K3S_1050 a special need for developing arrival date
       //  field SP_PREFSEQ is used to sequence diverter entries, but not
       //  field SO_PREFSEQ. Look at logical K_INTALTRD to confirm
       //  since field IA_PREFSEQ is used in RPGLE program K3S_M071
          so_prefseq = *zeros;
          if vary_orcyc = 1 and
             @u_dowmap  = 1 and
             si_rectype = 'D';

             so_prefseq = 555;
          endif;

          so_reqsplt = sp_reqsplt;
          clear so_recalc;
          clear so_po#;
          clear so_group;
          clear so_spl1typ;
          clear so_spl1prt;
          clear so_spl2typ;
          clear so_spl2prt;
          clear so_usera2;

       // if alternate source order, order cycle had been earlier saved
       //    this is necessary because individual products needed to use
       //    their regular source order cycles, to calculate correct sfty stk
          if alt_sour = 1;
             so_orcycle = savorcycle;
          else;
             so_orcycle = sp_orcycle;
          endif;

       // if alternate source order, generate the Regular cost total fields
          if alt_sour = 1;
             so_replreg = so_replnet + so_reploth;
             so_actureg = so_actunet + so_actuoth;
          endif;

       // if alternate source OR variable order cycle,
       //    OR a Fixed Cycle supplier, set
       //    Order Cycle code to display as normal
          if alt_sour = 1   or
             vary_orcyc = 1 or
             so_fxcfrq  > 0;

             so_usera1 = '1';

          else;

       //   look through Supplier Change Log for answer
             OCA_recent = *blanks;

             callp K3S_2009(sp_comp:
                            sp_locn:
                            sp_supl:
                            sp_suplsub:
                            OCA_recent);

             so_usera1 = '0';
             if OCA_recent <> *blanks;
                so_usera1 = '1';
                recent_ISO = %date(OCA_recent:*iso);
                OCA_days = %diff(lc_sysdate:recent_ISO:*days);
                if OCA_days > 730;
                   so_usera1 = '4';
                else;
                   if OCA_days > 365;
                      so_usera1 = '3';
                   else;
                      if OCA_days > 180;
                         so_usera1 = '2';
                      endif;
                   endif;
                endif;
             endif;
          endif;

       // this is a Host Supplier, then set records to temporary
          if sp_altsrce = 2;
             so_tempory = 1;
          endif;

       // discount bracket logic

          so_cur1lev = sp_discbkt;
          so_rebldop = sp_discbkt;
          discbkt_ind = *off;

       // supplier does not have any active discount brackets
          if so_cur1lev = 0;
             clear so_cur1typ;
             clear so_cur1val;
             clear so_cur1unt;
             clear so_cur2typ;
             clear so_cur2val;
             clear so_cur2unt;
             clear so_currate;
             clear so_cursavg;
             clear so_nxt1lev;
             clear so_nxt1typ;
             clear so_nxt1val;
             clear so_nxt1unt;
             clear so_nxt2typ;
             clear so_nxt2val;
             clear so_nxt2unt;
             clear so_nxtrate;
             clear so_nxtsavg;
             discbkt_ind = *on;

       //  go get supplier bracket information
          else;

       //  current bracket
             exec sql
               select sd_discbkt, sd_dis1typ, sd_dis1val, sd_dis1unt,
                      sd_dis2typ, sd_dis2val, sd_dis2unt,
                      sd_disrate, sd_dissavg
               into   :sddiscbkt, :sddis1typ, :sddis1val, :sddis1unt,
                      :sddis2typ, :sddis2val, :sddis2unt,
                      :sddisrate, :sddissavg
               from k_supldis
               where sd_comp = :sp_comp and        //pr
                     sd_locn = :sp_locn and        //pr
                     sd_supl = :sp_supl and        //pr
                     sd_suplsub = :sp_suplsub and  //pr
                     sd_discbkt = :sp_discbkt;
             if SQLState = SQLStateOk;
                so_cur1typ = sddis1typ;
                so_cur1val = sddis1val;
                so_cur1unt = sddis1unt;
                so_cur2typ = sddis2typ;
                so_cur2val = sddis2val;
                so_cur2unt = sddis2unt;
                so_currate = sddisrate;
                so_cursavg = sddissavg;
                discbkt_ind = *on;
             endif;

       //    look for 2nd bracket existance
       //      but not at the last bracket!
             if sddiscbkt < 9;
                #sddiscbkt = sddiscbkt + 1;
                exec sql
                  select sd_dis1typ, sd_dis1val, sd_dis1unt,
                         sd_dis2typ, sd_dis2val, sd_dis2unt,
                         sd_disrate, sd_dissavg
                  into   :sddis1typ, :sddis1val, :sddis1unt,
                         :sddis2typ, :sddis2val, :sddis2unt,
                         :sddisrate, :sddissavg
                  from k_supldis
                  where sd_comp = :sp_comp and        //pr
                        sd_locn = :sp_locn and        //pr
                        sd_supl = :sp_supl and        //pr
                        sd_suplsub = :sp_suplsub and  //pr
                        sd_discbkt = :#sddiscbkt;
                if SQLState = SQLStateOk;

       //         next level bracket
                   so_nxt1lev = #sddiscbkt;
                   so_nxt1typ = sddis1typ;
                   so_nxt1val = sddis1val;
                   so_nxt1unt = sddis1unt;
                   so_nxt2typ = sddis2typ;
                   so_nxt2val = sddis2val;
                   so_nxt2unt = sddis2unt;
                   so_nxtrate = sddisrate;
                   so_nxtsavg = sddissavg;
                   discbkt_ind = *on;
                else;
                   clear so_nxt1lev;
                   clear so_nxt1typ;
                   clear so_nxt1val;
                   clear so_nxt1unt;
                   clear so_nxt2typ;
                   clear so_nxt2val;
                   clear so_nxt2unt;
                   clear so_nxtrate;
                   clear so_nxtsavg;
                endif;
             endif;

       //    end test on  so_cur1lev = 0
          endif;

       //-------------------------------------------------------- Delay days
       // calculate delay days for supplier order

       //   only perform calculations if a specified value exists
          if specoutt > 0;

             delay = 0;
             foundit = *off;
             i = 2;

             dou (foundit = *on) or (i = 7);

                if (expstkoutt(i) > specoutt);
                   foundit = *on;
                else;
                   i += 1;
                   delay += 1;
                endif;

             enddo;

          endif;

          if (alt_sour = 0) and
             (so_actureg = 0) and
             (delay = 0);

             delay = 5;
          endif;

          exptout = %xfoot(expstkoutt);

          if (exptout  = 0)   and
             (so_actureg > 0) and
             (delay = 0)      and
             (specoutt = 0)   and
             (so_chkopnt = 0);

             delay = 5;
          endif;

          so_joint = delay;

       //     order type 'ND' for no delay
          if so_joint = 0;
             so_soqtype = 'ND';
          endif;

       //     alternate source orders are automatically set to no delay
          if so_altsrce = 1;
             so_joint   = 0;
             so_soqtype = 'ND';
          endif;

       //if using Weekly Distribution logic,
       //   then determine the DELAY days
       //   if even 1 product is needed, set to NO DELAY
          if vary_orcyc = 1 AND
             @u_dowmap  = 1;
             if so_chksoq  > 0;
                so_joint   = 0;
                so_soqtype = 'ND';
             else;
                so_joint   = 1;
                so_soqtype = '  ';
             endif;
          endif;

       // if using Variable Order Cycle suppliers,
       //    and customer does not want to see orders prompted early,
       //    and this is NOT an order day (SI_ORCYCxx = 0)
       //    then delay the order
          if vary_orcyc = 1 and
             DELAY_VOC  = 1 and
             NOT_TODAY  = 1;
             so_joint   = 1;
             so_soqtype = '  ';
          endif;

       //  fixed cycle logic
          if so_fxcfrq  > 0;
       //       this is the day
             if so_fxcnxt  = lc_sysdate;
                so_soqtype = 'FC';
             endif;
       // next Fixed Day coming up, but order due now
       // default is to delay another day and don't
       // display order early
             if so_fxcnxt  > lc_sysdate and
                so_joint   = 0          and
                so_soqtype = 'ND'       and
                DELAY_FC   = 1;
                so_joint   = 1;
                so_soqtype = '  ';
             endif;
       //       we missed the day
             if so_fxcnxt  < lc_sysdate;
             so_soqtype = 'FR';
             endif;
          endif;

       //  RE Reminder type
       //       only when soq type is blank, would we be involved with
       //       coding entire order as RE. otherwise, we assume buyer
       //       would be looking at order anyway.
          if so_soqtype = *blanks AND check_RE = 1 AND
             alt_sour = 0;
       //       do product reminders exist ?
             if so_chkremi > 0;
                so_soqtype = 'RE';
             else;
       //       no, how about supplier reminders ?

       //--- start supplier reminders logic

       //   there are two combinations that can occur for supplier reminders
       //   if the first Type condition occurs, the remaining Type is not
       //   checked
       //                    Location
       //      Type 1 -        Yes
       //      Type 2 -        blank

       //   Type 1 Start ------------------------
                typval_nt = 'S';
                prod_nt = *blanks;
                deal_nt = *blanks;
                seasonl_nt = *blanks;

       //     generate key for testing
       //        combined supplier
       //                if        sp_altsrce = 2
       //                eval      locn_nt    = so_cmblocn
       //                eval      supl_nt    = so_cmbsupl
       //                eval      suplsub_nt = so_cmbsub

       //        regular supplier
       //                else
                locn_nt    = so_locn;
                supl_nt    = so_supl;
                suplsub_nt = so_suplsub;
       //                endif

       //     first test, does a reminder record exist?

       //     if yes, then see if we have reached reminder date,
       //             and that expire date in future
                 exec sql
                   select count(*)
                     into :notepad_count
                     from k_notepad
                     where nt_comp = :pr_comp and
                           nt_typval = :typval_nt and
                           nt_locn = :locn_nt and
                           nt_supl = :supl_nt and
                           nt_suplsub = :suplsub_nt and
                           nt_prod = :prod_nt and
                           nt_deal = :deal_nt and
                           nt_seasonl = :seasonl_nt and
                           nt_typnote = :typnote_RE;

                 if notepad_count > 0;

                    exsr InzInpSrchnt;
    ‚   //initialize StmtString
                    exsr IntSQLStmtnt;

    ‚                exsr PrepDynSQLStmtnt;

                    if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                       exsr opnntcursor;

                       dow SQLState = SQLStateOk;

                           exec sql
                             fetch next
                                from ntcursor
                                into :notepad_rec;

       //     if we have reached reminder date, get out


       //     if we have reached reminder date, get out
                           if SQLState = SQLStateOk;
                              if nt_remind <=  lc_sysdate;

                                 if (nt_expire > first_day AND
                                     nt_expire > lc_sysdate) OR
                                     (nt_expire = first_day);

                                     so_soqtype = 'RE';
                                     leave;
                                 endif;
                              endif;
                           endif;
                       enddo;
                       exsr clsntcursor;
                    endif;
                 endif;
       //      *in22 test
       //   Type 1 End   ------------------------

       //   Type 2 Start ------------------------
       //      no need to continue if reminder already discovered
                if so_soqtype <> 'RE';
                   typval_nt = 'S';
                   prod_nt = *blanks;
                   deal_nt = *blanks;
                   seasonl_nt = *blanks;

       //      generate key for testing
       //         combined supplier
       //                if        sp_altsrce = 2
       //                eval      locn_nt    = *blanks
       //                eval      supl_nt    = so_cmbsupl
       //                eval      suplsub_nt = so_cmbsub

       //         regular supplier
       //                else
                   locn_nt    = *blanks;
                   supl_nt    = so_supl;
                   suplsub_nt = so_suplsub;
       //                endif

       //     first test, does a reminder record exist?

       //     if yes, then see if we have reached reminder date,
       //             and that expire date in future
                   exec sql
                     select count(*)
                       into :notepad_count
                       from k_notepad
                       where nt_comp = :pr_comp and
                             nt_typval = :typval_nt and
                             nt_locn = :locn_nt and
                             nt_supl = :supl_nt and
                             nt_suplsub = :suplsub_nt and
                             nt_prod = :prod_nt and
                             nt_deal = :deal_nt and
                             nt_seasonl = :seasonl_nt and
                             nt_typnote = :typnote_RE;

                   if notepad_count > 0;

                      exsr InzInpSrchnt;
    ‚   //initialize StmtString
                      exsr IntSQLStmtnt;

    ‚                  exsr PrepDynSQLStmtnt;

                      if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                         exsr opnntcursor;

                         dow SQLState = SQLStateOk;

                             exec sql
                               fetch next
                                 from ntcursor
                                 into :notepad_rec;

       //     if we have reached reminder date, get out
                             if SQLState = SQLStateOk;
                                if nt_remind <=  lc_sysdate;

                                   if (nt_expire > first_day AND
                                      nt_expire > lc_sysdate) OR
                                      (nt_expire = first_day);
                                      so_soqtype = 'RE';
                                      leave;
                                   endif;
                                endif;
                             endif;
                         enddo;
                         exsr clsntcursor;
                      endif;
                   endif;
                endif;
       //        *in22 test
       //        so_soqtype <> 'RE'
       //   Type 2 End   ------------------------

             endif;
       //     so_chkremi > 0

          endif;
       //   so_soqtype = *blanks AND check_RE = 1 AND alt_sour = 0

       //-------------------------------------------------------------------

       //    All frozen and forecast = 0 testing
       //        not diverter order
       //        all products were 'F' Frozen
       //        all products have forecast = 0
       //        and at least 1 product needs to be ordered
          if so_altsrce <> 1 and
             so_chkrevu = all_froz_0 and
             so_chksoq  > 0;

             so_joint   = 0;
             so_soqtype = 'ND';
          endif;

       //   If buyer is re-calculating a suggested order while
       //       viewing the order, make sure new suggested order
       //       has Order Busy Flag on

             if day_time   = 1  and
                came_from  > 0;
                    so_ordbusy = 1;
                    so_lckuser = psds_user;
                    callp K3S_Retrieve_Timestamp(time_stamp);
                    so_lcktime = %time(time_stamp);
             endif;

          exsr insert_suplsoq;

          #cur1val = so_cur1val;
          #cur1unt = so_cur1unt;

       //------------------------------ build active products value at night

       //  If this is a night time run for regular orders only (not alternate
       //    source orders), then update the 'active products' field in
       //    supplier record.
       //  Include Combined Suppliers for updating 'active products' field
       //  -- all products within supplier that have cost $0,and using min/max,
       //  --   and fixed cycle set up, go advance to next fixed cycle date

          if (day_time = 0 AND alt_sour = 0) OR
             (sp_altsrce = 2);

       //  get supplier record

             exec sql
               select *
                 into :suplier_rec
                 from k_suplier
                 where sp_comp =:pr_comp and
                       sp_locn = :pr_locn and
                       sp_supl = :pr_supl and
                       sp_suplsub = :pr_suplsub
                 fetch first row only;

             if SQLState = SQLStateOk;

                sp_actprod = so_chkrevu;

                if all_cost_0 = all_minmax and
                   all_cost_0 > 0          and
                   all_cost_0 = so_chkrevu and
                   so_soqtype = 'FC';

                   all_days = so_fxcfrq * 7;
                   sp_fxcnxt += %days(all_days);
                endif;

       //  if order is not Due, reset Days left on Due screen counter
                if so_joint  > 0;
                   sp_usern1 = 0;
                endif;

                exec sql
                  update k_suplier
                    set sp_actprod = :sp_actprod,
                        sp_fxcnxt  = :sp_fxcnxt,
                        sp_usern1  = :sp_usern1
                    where sp_comp = :pr_comp and
                          sp_locn = :pr_locn and
                          sp_supl = :pr_supl and
                          sp_suplsub = :pr_suplsub;
             endif;

          endif;

       //------------------------------------------------ Allocation needed?

       //  Two questions must be answered below. do we need to allocate this
       //    order? and why we need to allocate. Is it because order doesn't
       //    reach supplier discount bracket, or because of a forward buy
       //    check?

          allocate = *off;
          fbuy_check = 0;

       //  if forward buy check active for this supplier order, allocate
       //       extra days
          if so_chkfbuy > 0;
             allocate = *on;
             fbuy_check = 1;
          endif;

          if discbkt_ind = *off;
             so_cur1val = #cur1val;
             so_cur1unt = #cur1unt;
          endif;

       //  if order delay = 0, and this is not a forward buy check order,
       //       and a rebuild option has been established for this supplier,
       //       then determine if order needs to be allocated to reach
       //       supplier discount brackets.
          if (delay = 0) and
             (allocate = *off) and
             (so_rebldop > 0);

       //       get value to test based upon supplier unit being used
             exsr $_getvalue;

       //         minimum criteria uses test for reaching level
             if (so_cur1typ = 0) and
                (testvalue < so_cur1val);

                allocate = *on;
             endif;

       //           maximum criteria automatically goes to allocation
             if so_cur1typ = 1;
                allocate = *on;
             endif;

          endif;


       //*****************************
       //  Fixed Cycle and Delay > 0 section
       //       the purpose of this section is to make sure that for FCs,
       //       when it is the day to order (FC), and an order does exist ($),
       //       and the supplier does have a discount bracket, and the order
       //       delay is not 0 (otherwise checked above), and the allocate
       //       flag was not previously turned on, then go through the
       //       tests to see if allocation needed
          if so_soqtype = 'FC' AND
             so_actunet > 0    AND
             so_rebldop > 0    AND
             delay      > 0    AND
             allocate   = *off;

       //       get value to test based upon supplier unit being used
             exsr $_getvalue;

       //           minimum criteria uses test for reaching level
             if (so_cur1typ = 0) and
                (testvalue < so_cur1val);

                allocate = *on;
             endif;

       //           maximum criteria automatically goes to allocation
             if so_cur1typ = 1;
                allocate = *on;
             endif;

          endif;
       //********************************


       //  go to allocation program if:
       //    1) allocation needed, and
       //       supplier constraint flag is 1
       //            or
       //    2) alternate source order flag is 1

          if (allocate   = *on)  and
             (supl_const = 1) or
             (alt_sour = 1);

       //  call to module for allocation
             program = 'K3S_1500';
             extra_days = 0;
             callp K3S_1510(program:
                            alt_sour:
                            so_comp:
                            so_buyr:
                            so_locn:
                            so_supl:
                            so_suplsub:
                            so_soqseq#:
                            fbuy_check:
                            extra_days:
                            keepqty);
          endif;

       //-------------------------------------------------------------------

       //    U1 - U4 Check processing exit
          if check_u = 1;
             callp K3S_1530(alt_sour:
                            so_comp:
                            so_buyr:
                            so_locn:
                            so_supl:
                            so_suplsub:
                            so_soqseq#);
          endif;

       endif;

       endsr;

       //* ////////////////////////////////////////////////// Remove old orders

       begsr $_rmvsoq;

       // position file cursor to last order

       // delete last order, if it has not been approved

       //    if from KS_1010, then DON'T remove other active orders
       //       (when this is Store Level environment, remove orders)
       exec sql
         delete
           from k_suplsoq
           where so_comp = :pr_comp and
                 so_buyr = :pr_buyr and
                 so_locn = :pr_locn and
                 so_supl = :pr_supl and
                 so_suplsub = :pr_suplsub and
                 so_soqseq# = :savsoqseq# and
                 :came_from = 0 and
                 so_soqtype <> 'AP' and
                 so_ordbusy <> 1 or
                 so_comp = :pr_comp and
                 so_buyr = :pr_buyr and
                 so_locn = :pr_locn and
                 so_supl = :pr_supl and
                 so_suplsub = :pr_suplsub and
                 so_soqseq# = :savsoqseq# and
                 so_soqtype <> 'AP' and
                 :rm_prv_ord = 1;

       //    if from K3S_1020, don't be concerned with other busy flag
       //       (when this is NOT Store Level environment, remove orders)
       exec sql
         delete
           from k_suplsoq
           where so_comp = :pr_comp and
                 so_buyr = :pr_buyr and
                 so_locn = :pr_locn and
                 so_supl = :pr_supl and
                 so_suplsub = :pr_suplsub and
                 so_soqseq# = :savsoqseq# and
                 :came_from > 0 and
                 so_soqtype <> 'AP' and
                 :rm_prv_ord = 0;

       // remove other orders

                 exsr InzInpSrchso;
    ‚   //initialize StmtString
                 exsr IntSQLStmtso;

    ‚             exsr PrepDynSQLStmtso;
       //if good sql statement prep
                 if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                    exsr opnsocursor;

                    dow SQLState = SQLStateOk;

                        exec sql
                          fetch next
                             from socursor
                             into :suplsoq_rec;
       // if row not found
                           if SQLState = RowNotFound;
                              leave;
                           else;
       //     if we have reached reminder date, get out
       // remove loop

       //    if from K3S_1010, then don't remove other active orders
       //       (when this is Store Level environment, remove orders)
                              if (came_from   = *zeros AND
                                 so_soqtype <> 'AP'   AND
                                 so_ordbusy <> 1)            OR
                                 (so_soqtype <> 'AP'   AND
                                 rm_prv_ord = 1);

                                 exec sql
                                   delete
                                     from k_suplsoq
                                     where current of socursor;

                              endif;
       //    if from K3S_1020, don't be concerned with other busy flag
       //       (when this is NOT Store Level environment, remove orders)
                              if came_from   > *zeros AND
                                 so_soqtype <> 'AP'   AND
                                 rm_prv_ord = 0;

                                 exec sql
                                   delete
                                     from k_suplsoq
                                     where current of socursor;
                              endif;
                           endif;
                           SQLState = SQLStateOk;
                    enddo;
                    exsr clssocursor;
                 endif;
       endsr;

       ////////////////////////////////////////////////// Expected Stock Out

       begsr $_expected;

       //     if regular order, include add days from supplier and product
       if alt_sour = 0;
          time = time
               + sp_add_day
               + pq_add_day;
       endif;

       //     take portion of order cycle
       part_cycle = 0;

       if pr_lstordr <> first_day AND
          pr_qtybaln > 0;

          diff_days = %diff(lc_sysdate:pr_lstordr:*days);
          if diff_days  >= 0 AND
             diff_days  <  sp_orcycle;

             part_cycle = (1 - (diff_days/sp_orcycle))
                              * pr_sstimef;
          endif;
       endif;
       tx = pq_leadtm + time - part_cycle;

       if tx < 0;
          tx = 1;
       endif;

       timesave = tx;

       clear @u_use;
       @u_days    = tx;

       callp K3S_Calc_Usage(@u_comp:
                            @u_seasonl:
                            @u_days:
                            @u_forcast:
                            @u_forcper:
                            @u_forcint:
                            @u_longtrm:
                            @u_dowmap:
                            @u_dowper1:
                            @u_dowper2:
                            @u_dowper3:
                            @u_dowper4:
                            @u_dowper5:
                            @u_dowper6:
                            @u_dowper7:
                            @u_sstper1:
                            @u_sstper2:
                            @u_sstper3:
                            @u_sstper4:
                            @u_sstper5:
                            @u_sstper6:
                            @u_sstper7:
                            @u_diff12:
                            @u_diff13:
                            @u_diff52:
                            @u_use:
                            @u_Ds_flag:
                            @u_DayNbr:
                            @u_ix:
                            @u_Ds_days:
                            @u_Ds_wdis:
                            @u_Ds_dlyu:
                            @u_Ds_spct:
                            @u_Ds_susg:
                            @u_Ds_accu:
                            @u_Ds_prno:
                            @u_Ds_fctr:
                            @u_Ds_warn);
       //exsr $_usage;

       cx = @u_use;

       if pr_qtybaln <= 0;
          eval(h) exptout = pq_costeac * cx;
          leavesr;
       endif;

       clear @u_use;
       @u_days = timesave + pr_devtimf;

       callp K3S_Calc_Usage(@u_comp:
                            @u_seasonl:
                            @u_days:
                            @u_forcast:
                            @u_forcper:
                            @u_forcint:
                            @u_longtrm:
                            @u_dowmap:
                            @u_dowper1:
                            @u_dowper2:
                            @u_dowper3:
                            @u_dowper4:
                            @u_dowper5:
                            @u_dowper6:
                            @u_dowper7:
                            @u_sstper1:
                            @u_sstper2:
                            @u_sstper3:
                            @u_sstper4:
                            @u_sstper5:
                            @u_sstper6:
                            @u_sstper7:
                            @u_diff12:
                            @u_diff13:
                            @u_diff52:
                            @u_use:
                            @u_Ds_flag:
                            @u_DayNbr:
                            @u_ix:
                            @u_Ds_days:
                            @u_Ds_wdis:
                            @u_Ds_dlyu:
                            @u_Ds_spct:
                            @u_Ds_susg:
                            @u_Ds_accu:
                            @u_Ds_prno:
                            @u_Ds_fctr:
                            @u_Ds_warn);
       //exsr $_usage;

       bx = @u_use - cx;

       if bx = 0;
          exptout = 0;
          leavesr;
       endif;

       eval(h) cx = (pr_qtybaln - cx) / bx;

       if cx > 4;
          exptout  = 0;
          if alt_sour = 0;
             backout = *on;
          endif;
          leavesr;
       endif;

       if cx < -3;
          cx = cx * -1;
          eval(h) bx = cx * bx;
          eval(h) exptout = pq_costeac * bx;
          leavesr;
       endif;

       eval(h) kkk = 4.1 - cx;
       eval(h) @g_k = 10  * kkk;
       eval(h) bx  = pq_costeac * bx;

       @g_g = meandevary(@g_k);

       eval(h) exptout = bx * @g_g;

       endsr;

       /////////////////////////////////////////// Clear Supplier SOQ values

       begsr $_clrsuplq;

       //     clear supplier soq values for next order

       clear so_chkrevu;
       clear so_chkback;
       clear so_chkchg;
       clear so_chkcont;
       clear so_chkdeal;
       clear so_chkdisc;
       clear so_chkfbuy;
       clear so_chkfbfl;
       clear so_chkfrez;
       clear so_chkmanl;
       clear so_chknew;
       clear so_chkopnt;
       clear so_chkover;
       clear so_chkprob;
       clear so_chkremi;
       clear so_chksoq;
       clear so_chkserv;
       clear so_chk6mon;
       clear so_chkwatc;
       clear so_chkusr1;
       clear so_chkusr2;
       clear so_chkusr3;
       clear so_chkusr4;
       clear so_replreg;
       clear so_replnet;
       clear so_replwgt;
       clear so_replvol;
       clear so_replpqt;
       clear so_reploth;
       clear so_replun7;
       clear so_replun8;
       clear so_replun9;
       clear so_actureg;
       clear so_actunet;
       clear so_actuwgt;
       clear so_actuvol;
       clear so_actupqt;
       clear so_actuoth;
       clear so_actuun7;
       clear so_actuun8;
       clear so_actuun9;
       clear so_joint;
       clear so_jointex;
       clear so_rebldop;
       clear so_soqtype;
       clear so_ordbusy;

       clear expstkoutt;
       clear specoutt;

       clear all_cost_0;
       clear all_minmax;

       clear all_froz_0;

       //Below are mods added 06/30/2021 to try to prevent decimal data error.

       clear so_altsrce;
       clear so_tempory;
       clear so_fxcfrq;
       clear so_fxcday;
       clear so_headcst;
       clear so_leadtmq;
       clear so_leadtmo;
       clear so_leadtmv;
       clear so_orcycle;
       clear so_service;
       clear so_maxdays;
       clear so_add_day;
       clear so_precise;
       clear so_autopo;
       clear so_recalc;
       clear so_cur1lev;
       clear so_cur1typ;
       clear so_cur1val;
       clear so_cur1unt;
       clear so_cur2typ;
       clear so_cur2val;
       clear so_cur2unt;
       clear so_currate;
       clear so_cursavg;
       clear so_nxt1lev;
       clear so_nxt1typ;
       clear so_nxt1val;
       clear so_nxt1unt;
       clear so_nxt2typ;
       clear so_nxt2val;
       clear so_nxt2unt;
       clear so_nxtrate;
       clear so_nxtsavg;
       clear so_dsptot1;
       clear so_dsptot2;
       clear so_rebate;
       clear so_cashdsc;
       clear so_prefseq;
       clear so_reqsplt;
       clear so_repcary;
       clear so_usern1;
       clear so_usern2;
       clear so_usern3;
       clear so_chkalts;

       //End of mods added 06/30/2021


       endsr;

       ///////////////////////////// Clear and initialize product soq record

       begsr $_clrprodq;

       //    clear and initialize product soq record

       // clear checks
       clear pq_chkrevu;
       clear pq_chkback;
       clear pq_chkchg;
       clear pq_chkcont;
       clear pq_chkdeal;
       clear pq_chkdisc;
       clear pq_chkfbuy;
       clear pq_chkfbfl;
       clear pq_chkfrez;
       clear pq_chkmanl;
       clear pq_chknew;
       clear pq_chkopnt;
       clear pq_chkover;
       clear pq_chkprob;
       clear pq_chkremi;
       clear pq_chksoq;
       clear pq_chkserv;
       clear pq_chk6mon;
       clear pq_chkwatc;
       clear pq_chkusr1;
       clear pq_chkusr2;
       clear pq_chkusr3;
       clear pq_chkusr4;

       clear pq_restflg;
       clear pq_soqovrd;
       clear pq_soqact;
       clear pq_soqpcnt;
       clear pq_opointu;
       clear pq_opointd;
       clear pq_oruptou;
       clear pq_oruptod;
       clear pq_othdays;
       clear pq_delay;
       clear pq_fbxdays;
       clear pq_dolrext;
       clear pq_sstfday;
       clear pq_nonstck;

       clear pq_soqsso;
       clear pq_soqeso1;
       clear pq_soqeso2;
       clear pq_soqeso3;
       clear pq_soqeso4;
       clear pq_soqeso5;
       clear pq_soqeso6;

       pq_selfbpi = 0;
       pq_selfbds = 0;
       pq_selfbmd = 0;
       pq_selfbxd = 0;
       pq_poqtydv = 0;
       pq_orcycle = 0;
       pq_repcary = 0;
       pq_usern1  = 0;
       pq_usern2  = 0;
       pq_usern3  = 0;
       pq_mfgout  = 0;
       pq_chkalts = 0;
       pq_oruptox = 0;
       pq_intrval = 0;
       pq_sstimef = 0;


       // initialize fields
       pq_comp    = pr_comp;
       pq_leadtm  = pr_leadtm;
       pq_leadtmv = pr_leadtmv;
       pq_add_day = pr_add_day;

       pq_linecst = pr_linecst;
       pq_suplusr = pr_suplusr;
       pq_suplusb = pr_suplusb;

       pq_suplorg = pr_suplorg;
       pq_suplors = pr_suplors;
       pq_suplorn = sp_name;

       pq_locn = pr_locn;
       pq_supl = pr_supl;
       pq_suplsub = pr_suplsub;

       // if this is alternate source order, then get the original
       //    supplier name
       if alt_sour = 1;
          supl_name = *blanks;
          last_recd = 0;
          callp K3S_M150(pq_comp:
                         pq_locn:
                         pq_suplorg:
                         pq_suplors:
                         supl_name:
                         last_recd);       //last record off
          pq_suplorn = supl_name;
       endif;

       pq_qtyohnd = pr_qtyohnd;
       pq_qtyoord = pr_qtyoord;
       pq_qtyback = pr_qtyback;
       pq_qtyhold = pr_qtyhold;
       pq_qtypend = pr_qtypend;
       pq_qtybsum = 0;

       pq_prombeg = pr_prombeg;
       pq_promend = pr_promend;
       pq_promqty = pr_promqty;

       //  if buying multiple over-ridden from night time interface,
       //     then set flag to inform buyer
       pq_buymult = pr_buymult;
       if pr_buymult <> pr_buymuli;
          pq_buyovrd = 1;
       else;
          pq_buyovrd = 0;
       endif;

       pq_minqty  = pr_minqty;
       pq_convpak = pr_convpak;
       pq_convpkp = pr_convpkp;

       pq_weight  = pr_weight;
       pq_weightd = pr_weightd;
       pq_volume  = pr_volume;
       pq_volumed = pr_volumed;
       pq_purincr = pr_purincr;
       pq_disothr = pr_disothr;

       pq_buyr    = pr_buyr;
       pq_regn    = pr_regn;
       pq_prod    = pr_prod;
       pq_prodseq = pr_prodseq;
       pq_desc1   = pr_desc1;
       pq_packsiz = pr_packsiz;
       pq_birth   = lc_sysdate;
       pq_excuntl = pr_excuntl;
       pq_altsrce = pr_altsrce;

       pq_costdiv = pr_costdiv;
       pq_costeac = pr_costeac;
       pq_costord = pr_costreg;
       clear pr_brkcost;
       clear pr_brkflag;
       pq_brkflag = pr_brkflag;
       pq_brkcost = pr_brkcost;

       pq_usrstat = pr_usrstat;
       pq_sysstat = pr_sysstat;

       pq_forcast = pr_forcast;
       pq_forcint = pr_forcint;
       pq_forcper = pr_forcper;
       pq_longtrm = pr_longtrm;
       pq_seasonl = pr_seasonl;

       pq_deal    = pr_deal;
       pq_dealbeg = pr_dealbeg;
       pq_dealend = pr_dealend;

       pq_maminiu = pr_maminiu;
       pq_mamaxiu = pr_mamaxiu;
       pq_maminid = pr_maminid;
       pq_mamaxid = pr_mamaxid;

       pq_service = pr_service;
       pq_invmeth = pr_invmeth;
       pq_rebate  = pr_rebate;
       pq_maxdays = pr_maxdays;

       pq_overflg = pr_overflg;
       pq_overunt = pr_overunt;

       pq_cmblocn = pr_cmblocn;
       pq_cmbsupl = pr_cmbsupl;
       pq_cmbsub  = pr_cmbsub;

       pq_tempory = pr_tempory;

       // clear product work fields
       clear specout;
       clear exptout;
       clear expstkout;
       clear minday;
       clear maxday;
       clear minunit;
       clear maxunit;
       clear mnwork;
       clear mxwork;
       clear delay;

       endsr;

       ////////////////////////////////////////////////////////// Get value

       begsr $_getvalue;

       select;

          when so_cur1unt = 1;
               testvalue  = so_actureg;

          when so_cur1unt = 2;
               testvalue  = so_actunet;

          when so_cur1unt = 3;
               testvalue  = so_actuwgt;

          when so_cur1unt = 4;
               testvalue  = so_actuvol;

          when so_cur1unt = 5;
               testvalue  = so_actupqt;

          when so_cur1unt = 6;
               testvalue  = so_actuoth;

          when so_cur1unt = 7;
               testvalue  = so_actuun7;

          when so_cur1unt = 8;
               testvalue  = so_actuun8;

          when so_cur1unt = 9;
               testvalue  = so_actuun9;

       endsl;

       endsr;

       //////////////////////////////////////////// Log Product Transactions

       // Log of Product Transactions

       begsr $_log_prod;

       lg_comp    = pr_comp;
       lg_locn    = pr_locn;
       lg_buyr    = sp_buyr;
       lg_supl    = pr_supl;
       lg_suplsub = pr_suplsub;
       lg_suplusr = pr_suplusr;
       lg_suplusb = pr_suplusb;
       lg_suplnam = sp_name;
       lg_prod    = pr_prod;
       lg_desc1   = pr_desc1;

       // call module to retrieve timestamp
       callp K3S_Retrieve_Timestamp(time_stamp);
       lg_timestp = time_stamp;

       exsr insert_logprod;

       endsr;

       begsr $_loadarrays;

       for y = 1 to %elem(@g_gk);
            eval meandevary(y) = @g_gk(y);
       endfor;

       for y = 1 to %elem(@g_gk_s);
            eval strddevary(y) = @g_gk_s(y);
       endfor;

       endsr;

       Begsr IntSQLStmtpr;

       String = *blanks;
       String =   'Select * +
                   From K_Product +
                   Where';

       Sortseq = 'order by pr_comp, +
                           pr_locn, +
                           pr_supl, +
                           pr_suplsub, +
                           pr_prod +
                  for update of pr_deltcnt, +
                                pr_qtyhold, +
                                pr_qtybaln, +
                                pr_buymult, +
                                pr_safesum, +
                                pr_sstimef, +
                                pr_otimfac, +
                                pr_devtimf, +
                                pr_intrval, +
                                pr_soqnite';


       SelcritCL = 'pr_comp = ? and +
                    pr_altsrce = ?';       //Regular source supplier

       SelcritSP = 'pr_comp = ? and +
                    pr_buyr = ? and +
                    pr_locn = ? and +
                    pr_supl = ? and +
                    pr_suplsub = ? and +
                    pr_prod > ?';      //Build order from one supplier only

       SelcritAP = 'pr_comp = ? and +
                    pr_altsrce = ?';       //Alternate source supplier

       if pgm = 'K3S_NITECL';
          StmtString = %trim(String) + ' ' +
                       %trim(SelCritCL) + ' ' +
                       %trim(Sortseq);
       else;
          if pgm = 'K3S_1500SP';
             StmtString = %trim(String) + ' ' +
                          %trim(SelCritSP) + ' ' +
                          %trim(Sortseq);
          else;
             if pgm = 'K3S_1500AP';
                StmtString = %trim(String) + ' ' +
                             %trim(SelCritAP) + ' ' +
                             %trim(Sortseq);
             endif;
          endif;
       endif;

       endsr;

       //                       pr_safesum, +
       //                       pr_sstimef, +
       //                       pr_otimfac, +
       //                       pr_devtimf, +
       //                       pr_intrval, +
       //                       pr_soqnite';


       begsr PrepDynSQLStmtpr;
       exec sql
        Prepare DynSqlStmtpr
          From :StmtString;
       endsr;

       begsr dclprcursor;
       exec sql
        declare prcursor Cursor
         for DynSQLStmtpr;
       endsr;

       begsr opnprcursor;

       if pgm = 'K3S_NITECL';
          exec sql
            open prcursor
              using :comp,
                    :alt_sour;
       else;
          if pgm = 'K3S_1500AP';
             exec sql
               open prcursor
                 using :comp,
                       :alt_sour;
          else;
            if pgm = 'K3S_1500SP';
               exec sql
                 open prcursor
                   using :comp,
                         :buyr,
                         :location,
                         :suplier,
                         :suplsub,
                         :product;
            endif;
          endif;
       endif;

       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       Begsr IntSQLStmtpq;

       String = *blanks;
       String =   'Select * +
                   From K_Prodsoq +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr InzInpSrchpq;

       InpSrchCnd = *blanks;
       InpSrchCnd = 'pq_comp = ? and +
                     pq_buyr = ? and +
                     pq_locn = ? and +
                     pq_supl = ? and +
                     pq_suplsub = ? and +
                     pq_soqseq# = ? and +
                     pq_prod = ? +
                     Order by pq_comp, +
                              pq_buyr, +
                              pq_locn, +
                              pq_supl, +
                              pq_suplsub, +
                              pq_soqseq#, +
                              pq_prod';
       endsr;

       begsr dclpqcursor;
       exec sql
        declare pqcursor Cursor
         for DynSQLStmtpq;
       endsr;

       begsr PrepDynSQLStmtpq;
       exec sql
        Prepare DynSqlStmtpq
          From :StmtString;
       endsr;

       begsr opnpqcursor;
       exec sql
        open pqcursor
          using :pr_comp,
                :pr_buyr,
                :pr_locn,
                :pr_supl,
                :pr_suplsub,
                :soqseq#sav,
                :pr_prod;
       endsr;

       begsr clspqcursor;
       exec sql
        close pqcursor;
       endsr;

       begsr update_product;

       exec sql
        update k_product
          set pr_deltcnt = :pr_deltcnt,
              pr_qtyhold = :pr_qtyhold,
              pr_qtybaln = :pr_qtybaln,
              pr_buymult = :pr_buymult,
              pr_safesum = :pr_safesum,
              pr_sstimef = :pr_sstimef,
              pr_otimfac = :pr_otimfac,
              pr_devtimf = :pr_devtimf,
              pr_intrval = :pr_intrval,
              pr_soqnite = :pr_soqnite
          where current of prcursor;
       endsr;

       begsr dclpucursor;
       exec sql
        declare pucursor Cursor
         for DynSQLStmtpu;
       endsr;

       begsr PrepDynSQLStmtpu;
       exec sql
        Prepare DynSqlStmtpu
          From :StmtString;
       endsr;

       begsr opnpucursor;
       exec sql
        open pucursor
          using :pr_comp,
                :hold_locn,
                :hold_supl,
                :hold_sub,
                :pr_prod;
       endsr;

       Begsr IntSQLStmtpu;

       String = *blanks;
       String =   'Select * +
                   From K_Prodhld +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrchpu;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pu_comp = ? and +
                     pu_locn = ? and +
                     pu_supl = ? and +
                     pu_suplsub = ? and +
                     pu_prod = ? +
                     Order by pu_comp, +
                              pu_locn, +
                              pu_supl, +
                              pu_suplsub, +
                              pu_prod, +
                              pu_end';
       endsr;

       begsr clspucursor;
       exec sql
        close pucursor;
       endsr;

       begsr insert_prodsoq;

         exec sql
           insert into k_prodsoq
             values (:prodsoq_rec);

       endsr;

       begsr insert_logprod;

         exec sql
           insert into k_logprod
             values (:logprod_rec);

       endsr;

       begsr dclaacursor2;
       exec sql
        declare aacursor2 Cursor
         for DynSQLStmtaa2;
       endsr;

       Begsr IntSQLStmtaa;

       String = *blanks;
       String =   'Select * +
                   From K_Autoadd +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr InzInpSrchaa2;

       InpSrchCnd = *blanks;
       InpSrchCnd = 'aa_comp = ? and +
                     aa_rectype = ? and +
                     aa_locn = ? and +
                     aa_supl = ? and +
                     aa_suplsub = ? and +
                     aa_prod = ? +
                     Order by aa_comp, +
                              aa_rectype, +
                              aa_locn, +
                              aa_supl, +
                              aa_suplsub, +
                              aa_prod, +
                              aa_end';
       endsr;

       begsr PrepDynSQLStmtaa2;
       exec sql
        Prepare DynSqlStmtaa2
          From :StmtString;
       endsr;

       begsr opnaacursor2;
       exec sql
        open aacursor2
          using :pr_comp,
                :add_rectyp,
                :add_locn,
                :add_supl,
                :add_sub,
                :add_prod;
       endsr;

       begsr clsaacursor2;
       exec sql
        close aacursor2;
       endsr;

       begsr InzInpSrchaa1;

       InpSrchCnd = *blanks;
       InpSrchCnd = 'aa_comp = ? and +
                     aa_rectype = ? and +
                     aa_locn = ? and +
                     aa_supl >= ? and +
                     aa_suplsub >= ? and +
                     aa_prod >= ? +
                     Order by aa_comp, +
                              aa_rectype, +
                              aa_locn, +
                              aa_supl, +
                              aa_suplsub, +
                              aa_prod, +
                              aa_end';
       endsr;

       begsr opnaacursor1;
       exec sql
        open aacursor1
          using :pr_comp,
                :add_rectyp,
                :add_locn,
                :add_supl,
                :add_sub,
                :add_prod;
       endsr;

       begsr clsaacursor1;
       exec sql
        close aacursor1;
       endsr;

       begsr PrepDynSQLStmtaa1;
       exec sql
        Prepare DynSqlStmtaa1
          From :StmtString;
       endsr;

       begsr dclaacursor1;
       exec sql
        declare aacursor1 Cursor
         for DynSQLStmtaa1;
       endsr;

       begsr InzInpSrchpd;

       InpSrchCnd = *blanks;
       InpSrchCnd = 'pd_comp = ? and +
                     pd_actlocn = ? and +
                     pd_actsupl = ? and +
                     pd_actsub = ? and +
                     pd_prod = ? and +
                     pd_rectype = ? +
                     pd_birth >= ? +
                     Order by pd_comp, +
                              pd_actlocn, +
                              pd_actsupl, +
                              pd_actsub, +
                              pd_prod, +
                              pd_reqtype, +
                              pd_birth';
       endsr;

       Begsr IntSQLStmtpd;

       String = *blanks;
       String =   'Select * +
                   From K_Prodsed +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr dclpdcursor;
       exec sql
        declare pdcursor Cursor
         for DynSQLStmtpd;
       endsr;

       begsr PrepDynSQLStmtpd;
       exec sql
        Prepare DynSqlStmtpd
          From :StmtString;
       endsr;

       begsr opnpdcursor;
       exec sql
        open pdcursor
          using :pr_comp,
                :xx_locn,
                :xx_supl,
                :xx_suplsub,
                :pr_prod,
                :reqtype_pd,
                :ending_SET;
       endsr;

       begsr clspdcursor;
       exec sql
        close pdcursor;
       endsr;

       Begsr IntSQLStmtse;

       String = *blanks;
       String =   'Select * +
                   From K_Schedpe +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrchse;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'se_comp = ? and +
                     se_forcint = ? and +
                     se_procflg = 1 +
                     Order by se_comp, +
                              se_forcint desc, +
                              se_forcyr desc, +
                              se_forcper desc, +
                              se_procflg';
       endsr;

       begsr opnsecursor;
       exec sql
        open secursor
          using :pr_comp,
                :#forcint;
       endsr;

       begsr clssecursor;
       exec sql
        close secursor;
       endsr;

       begsr dclsecursor;
       exec sql
        declare secursor Cursor
         for DynSQLStmtse;
       endsr;

       begsr PrepDynSQLStmtse;
       exec sql
        Prepare DynSqlStmtse
          From :StmtString;
       endsr;

       Begsr IntSQLStmtnt;

       String = *blanks;
       String =   'Select * +
                   From K_Notepad +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr InzInpSrchnt;

       InpSrchCnd = *blanks;
       InpSrchCnd = 'nt_comp = ? and +
                     nt_typval = ? and +
                     nt_locn = ? and +
                     nt_supl = ? and +
                     nt_suplsub = ? and +
                     nt_prod = ? and +
                     nt_deal = ? and +
                     nt_seasonl = ? and +
                     nt_typnote = ? +
                     Order by nt_comp, +
                              nt_typval, +
                              nt_locn, +
                              nt_supl, +
                              nt_suplsub, +
                              nt_prod, +
                              nt_deal, +
                              nt_seasonl, +
                              nt_typnote';
       endsr;

       begsr dclntcursor;
       exec sql
        declare ntcursor Cursor
         for DynSQLStmtnt;
       endsr;

       begsr PrepDynSQLStmtnt;
       exec sql
        Prepare DynSqlStmtnt
          From :StmtString;
       endsr;

       begsr opnntcursor;
       exec sql
        open ntcursor
          using :pr_comp,
                :typval_nt,
                :locn_nt,
                :supl_nt,
                :suplsub_nt,
                :prod_nt,
                :deal_nt,
                :seasonl_nt,
                :typnote_RE;
       endsr;

       begsr clsntcursor;
       exec sql
        close ntcursor;
       endsr;

       begsr insert_suplsoq;

         exec sql
           insert into k_suplsoq
             values (:suplsoq_rec);

       endsr;

       Begsr IntSQLStmtso;

       String = *blanks;
       String =   'Select * +
                   From K_suplsoq +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr InzInpSrchso;

       InpSrchCnd = *blanks;
       InpSrchCnd = 'so_comp = ? and +
                     so_buyr = ? and +
                     so_locn = ? and +
                     so_supl = ? and +
                     so_suplsub = ? and +
                     so_soqseq# < ? +
                     Order by so_comp desc, +
                              so_buyr desc, +
                              so_locn desc, +
                              so_supl desc, +
                              so_suplsub desc, +
                              so_soqseq# desc';
       endsr;

       begsr dclsocursor;
       exec sql
        declare socursor Cursor
         for DynSQLStmtso;
       endsr;

       begsr PrepDynSQLStmtso;
       exec sql
        Prepare DynSqlStmtso
          From :StmtString;
       endsr;

       begsr opnsocursor;
       exec sql
        open socursor
          using :pr_comp,
                :pr_buyr,
                :pr_locn,
                :pr_supl,
                :pr_suplsub,
                :savsoqseq#;
       endsr;

       begsr clssocursor;
       exec sql
        close socursor;
       endsr;

       /////////////////////////////////////////////////////////////////////
      /end-free

      * ---------------------------------- calculate safety stock components
     c*copy k3s_c091

      * ---------------------------------- calculate usage (days into units)
     c*copy k3s_c101

      * --------------------------------- calculate suggested order quantity
     c*copy k3s_c121

      * ------------------------------------ calculate manual min/max values
     c*copy k3s_c131

      * ----------------------------------------- calculations with GK table
     c/copy k3s_c111
