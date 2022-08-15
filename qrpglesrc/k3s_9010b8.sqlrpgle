      *****************************************************************
     h copyright('(C) Copyright 1996 - 2014 King III Solutions, Inc.  +
     h Rel 5.1  2014-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO:*SRCSTMT)
     h BNDDIR('K3S_BNDDIR')
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
      **   Name: K3S_9010
      **   Type: ILE RPG Program
      **   Desc: Update database from external source
      **
      *****************************************************************
      **
      **  This program is used to update the database from an external
      **  source. Typically, this program would run once each night,
      **  and contain one record for each product to be updated.
      **
      **  This program will determine a change in forecasting period,
      **  and will automatically set on the forecasting module based
      **  upon this knowledge.
      **
      **  This program reads products from file k_intprod that were
      **  assembled using OPNQRYF. This OPNQRYF set should only contain
      **  data for one company at a time. Multiple locations can be
      **  processed during the same batch. For performance, make sure
      **  sequence is in company, location, supplier, sub supplier.
      **
      *****************************************************************
      **
      **  The file K_LOGPROD contains transactions that audit changes to
      **  products in the K3S-Replenish system. Most of these transactions
      **  will be created within this program. However, program K3S_1500
      **  is responsible for generating types 3 and 4.
      **
      **     The types are:  1  New product
      **                     2  Products changing supplier
      **                     3  No interface records passed
      **                     4  Products deleted from file
      **                     5  Supplier passed does not exist
      **                     6  Location passed does not exist
      **                     7  Products being re-instated
      **                     8  First time 'D' make 'M'
      **                     9  Probation period expired
      **
      **                     A  Forecast going negative
      **                     B  Deviation going negative
      **                     C  Invalid T/S generated
      **                     D  Negative sales passed
      **                     E  Negative outs passed
      **                     F  Negative on hand passed
      **                     G  Negative on order passed
      **                     H  Negative back orders passed
      **                     I  Negative purchase price passed
      **                     J  Negative sales price passed
      **                     K  Product missing description
      **                     L  Purchase price divisor < 1
      **                     M  Buy multiple passed < 1
      **                     N  Minimum quantity passed < 1
      **                     O  Negative purhase increment
      **                     Q  Negative convenience pack
      **                     R  Negative weight passed
      **                     S  Weight divisor less than 1
      **                     T  Negative volume passed
      **                     U  Volume divisor less than 1
      **                     V  Status passed not 'R' or 'D'
      **                     W  Forecast cycle code not 1,2,3
      **                     X  Invalid flag alt srce process
      **                     Y  Sales price < cost
      **                     Z  Negative rebate passed
      **
      **                     a  Conv Pack less than Buy Mult
      **                     b  Conv Pack not a true multiple
      **                     c  Cost sent across as $0
      **
      *****************************************************************
      **
      **  There are 3 supplier fields in the K3S-Replenish database
      **
      **      supl    - is used to build orders
      **
      **      suplorg - is used to go after history. since alternate source
      **                orders will have products under 'supl', the
      **                'suplorg' field directs us to the original
      **                supplier to know where the history records are.
      **
      **      suplusr - can be used to pass back orders for super
      **                suppliers. It should contain the supplier that
      **                the product exists under in the user's host
      **                system, and is sent across in k_intprod.
      **                Transfer supplier logic would change the ip_supl
      **                field, but not the ip_suplusr. the ip_suplusr
      **                is to carry throughout the entire system via
      **                pr_suplusr.

      *****************************************************************
      * -------------------------------------- LDA D specs
     d/COPY K3S_C035

      * ----------------------------------------- Supplier for Control Break
     d
     d supl            ds
     d  #iplocn                       5
     d  #ipsupl                      10
     d  #ipsuplsub                   10
     d suplsaved       s                   like(supl)

      * --------------------------------------------------------- Workfields
     d #once           s              1  0
     d #notfirst       s              1
     d exist_here      s              1
     d exist_othr      s              1
     d locn            s                   like(pr_locn)
     d forcper         s                   like(pr_forcper)
     d error_type      s              1
     d histype         s              1  0
     d demlo           s                   like(lc_demlo)
     d seas_factr      s              5  2                                      Seasonal factor
     d days_perd       s                   like(pr_daysout)
     d lost_perct      s              3  2
     d bad_suplr       s              1
     d bad_suplrd      s             25    inz('Supplier not in database')
     d bad_locn        s              1
     d bad_locnd       s             25    inz('Location not in database')
     d time_stamp      s               z   inz
     d block_alts      s              1  0                                      block alt source
     d new_slow        s              1  0                                      new to slow status
     d new_usedft      s              1  0                                      new use deflt avg?
     d new_avgdft      s              7  2                                      new default average
     d cost_0          s              1  0                                      cost sent as $0?
     d cost_fill       s              1  0                                      cost to be filled?
     d cost_value      s              7  2                                      cost default value
     d night           s              1                                         parm for K3S_5056
     d delay_days      s              1  0                                      delay days flg 1=yes
     d delay_diff      s              5  0                                      delay birth differnc
     d delayuntil      s               d                                        delay until date
     d delay_test      s                   like(pr_birth)                       delay until date
     d Ninety_Out      s               d                                        delay until date
     d copy_deals      s              1  0                                      copy deals flg 1=yes
     d trans_prod      s              1  0                                      trans prod flg 1=yes
     d no_deal_06      s              1  0                                      bw no deal '06'1=yes
     d min_season      s              7  2
     d min_used        s              1  0
     d clr_record      s              1  0                                      clear PR_EXCUNTL
     d clr_promo       s              1  0                                      clear 3 promo fields
     d not_0001        s               d   inz(d'0001-01-01') datfmt(*iso)
     d back_order      s              1  0                                      include back order
     d lost_sales      s              1  0                                      include lost sales
     d continue        s              1  0                                      continue with logic
     d lstrcvd         s              1  0                                      last receive date
     d days_10out      s               d                                        10 days out
     d corp_prof       s              1  0                                      use corporate prof?
     d CorpProfID      s                   like(pr_seasonl)                     default corp profile
     d NewBirth        s              1  0                                      Birth for new prods
     d car_update      s              1  0                                      Car Count update
     d PE1_FLTR_1      s              1  0                                      PE1 filtering
     d pe_snpshot      s              1  0                                      PE snapshots
     d first_shot      s              1  0                                      once routine
     d not_recvd       s               d   inz(d'0001-01-01') datfmt(*iso)
     d ref_birth       s              1  0                                      ref birth logic
     d taflag1         s              1  0
     d taflag2         s              1  0
     d tanumber1       s              5  0
     d tanumber2       s              7  2
     d cmsysdate       s               d
     d cmformeth       s              1  0
     d cminvmeth       s              1  0
     d tacodeds3       s            100
     d plordrecv       s               d
     d UpdQtyPend      s              1  0                                      Update PR_QTYPEND?
     d init_date       s               d   inz(d'0001-01-01') datfmt(*iso)
     d Idx             s             10i 0
     d Ndx             s             10i 0
     d RowsFetched     s             10i 0
      *
     d prcomp          s                   like(pr_comp)
     d prbuyr          s                   like(pr_buyr)
     d prregn          s                   like(pr_regn)
     d prlocn          s                   like(pr_locn)
     d prsupl          s                   like(pr_supl)
     d prsuplsub       s                   like(pr_suplsub)
     d prsuplusr       s                   like(pr_suplusr)
     d prsuplusb       s                   like(pr_suplusb)
     d prsuplorg       s                   like(pr_suplorg)
     d prsuplors       s                   like(pr_suplors)
     d prprod          s                   like(pr_prod)
     d praltsrce       s                   like(pr_altsrce)
     d prtempory       s                   like(pr_tempory)
     d prprodseq       s                   like(pr_prodseq)
     d prdesc1         s                   like(pr_desc1)
     d prdesc2         s                   like(pr_desc2)
     d prndc_upc       s                   like(pr_ndc_upc)
     d prmfg           s                   like(pr_mfg)
     d prcatalog       s                   like(pr_catalog)
     d pruom           s                   like(pr_uom)
     d prpacksiz       s                   like(pr_packsiz)
     d prtihi          s                   like(pr_tihi)
     d prgroup1        s                   like(pr_group1)
     d prgroup1o       s                   like(pr_group1o)
     d prgroup2        s                   like(pr_group2)
     d prgroup2o       s                   like(pr_group2o)
     d prgroup3        s                   like(pr_group3)
     d prgroup3o       s                   like(pr_group3o)
     d prgroup4        s                   like(pr_group4)
     d prgroup4o       s                   like(pr_group4o)
     d prgroup5        s                   like(pr_group5)
     d prgroup5o       s                   like(pr_group5o)
     d prwhslocn       s                   like(pr_whslocn)
     d prbirth         s                   like(pr_birth)
     d prrfbirth       s                   like(pr_rfbirth)
     d prlastupd       s                   like(pr_lastupd)
     d prlinecst       s                   like(pr_linecst)
     d prleadtm        s                   like(pr_leadtm)
     d prleadtmp       s                   like(pr_leadtmp)
     d prleadtmv       s                   like(pr_leadtmv)
     d prleadtms       s                   like(pr_leadtms)
     d prleadtmt       s                   like(pr_leadtmt)
     d prcostreg       s                   like(pr_costreg)
     d prcostdiv       s                   like(pr_costdiv)
     d prcosteac       s                   like(pr_costeac)
     d provrcreg       s                   like(pr_ovrcreg)
     d provrcdiv       s                   like(pr_ovrcdiv)
     d prcostlst       s                   like(pr_costlst)
     d prcostldt       s                   like(pr_costldt)
     d prbrkflag       s                   like(pr_brkflag)
     d prbrkcost       s                   like(pr_brkcost)
     d prsales         s                   like(pr_sales)
     d prsaleslw       s                   like(pr_saleslw)
     d prqtyohnd       s                   like(pr_qtyohnd)
     d prqtyoord       s                   like(pr_qtyoord)
     d prqtyback       s                   like(pr_qtyback)
     d prqtyhold       s                   like(pr_qtyhold)
     d prqtypend       s                   like(pr_qtypend)
     d prqtybaln       s                   like(pr_qtybaln)
     d prpromqty       s                   like(pr_promqty)
     d prprombeg       s                   like(pr_prombeg)
     d prpromend       s                   like(pr_promend)
     d prusrstat       s                   like(pr_usrstat)
     d prsysstat       s                   like(pr_sysstat)
     d prsoqnite       s                   like(pr_soqnite)
     d prforcast       s                   like(pr_forcast)
     d prformanl       s                   like(pr_formanl)
     d prcustusg       s                   like(pr_custusg)
     d prforfrez       s                   like(pr_forfrez)
     d prprobdat       s                   like(pr_probdat)
     d prfordevp       s                   like(pr_fordevp)
     d prforserr       s                   like(pr_forserr)
     d prforchg        s                   like(pr_forchg)
     d prminqty        s                   like(pr_minqty)
     d prminqtyo       s                   like(pr_minqtyo)
     d prbuymult       s                   like(pr_buymult)
     d prbuymulo       s                   like(pr_buymulo)
     d prbuymuli       s                   like(pr_buymuli)
     d prconvpak       s                   like(pr_convpak)
     d prconvpko       s                   like(pr_convpko)
     d prconvpkp       s                   like(pr_convpkp)
     d prdaysout       s                   like(pr_daysout)
     d prdaysprv       s                   like(pr_daysprv)
     d prdaysunt       s                   like(pr_daysunt)
     d printrval       s                   like(pr_intrval)
     d protimfac       s                   like(pr_otimfac)
     d prmaxdays       s                   like(pr_maxdays)
     d prmaminid       s                   like(pr_maminid)
     d prmamaxid       s                   like(pr_mamaxid)
     d prmaminiu       s                   like(pr_maminiu)
     d prmamaxiu       s                   like(pr_mamaxiu)
     d pradd_day       s                   like(pr_add_day)
     d pransale$       s                   like(pr_ansale$)
     d pransaleu       s                   like(pr_ansaleu)
     d prdevtimf       s                   like(pr_devtimf)
     d prsstimef       s                   like(pr_sstimef)
     d prsafesum       s                   like(pr_safesum)
     d prweight        s                   like(pr_weight)
     d prweighto       s                   like(pr_weighto)
     d prweightd       s                   like(pr_weightd)
     d prweighdo       s                   like(pr_weighdo)
     d prvolume        s                   like(pr_volume)
     d prvolumeo       s                   like(pr_volumeo)
     d prvolumed       s                   like(pr_volumed)
     d prvolumdo       s                   like(pr_volumdo)
     d prpurincr       s                   like(pr_purincr)
     d prpurinco       s                   like(pr_purinco)
     d prdisothr       s                   like(pr_disothr)
     d prseasonl       s                   like(pr_seasonl)
     d prseasact       s                   like(pr_seasact)
     d prseassrc       s                   like(pr_seassrc)
     d prservice       s                   like(pr_service)
     d prservsim       s                   like(pr_servsim)
     d prsvceprv       s                   like(pr_svceprv)
     d prforcint       s                   like(pr_forcint)
     d prforcyr        s                   like(pr_forcyr)
     d prforcper       s                   like(pr_forcper)
     d prfstslyr       s                   like(pr_fstslyr)
     d prfstslpr       s                   like(pr_fstslpr)
     d prhistper       s                   like(pr_histper)
     d prpromper       s                   like(pr_promper)
     d prlongtrm       s                   like(pr_longtrm)
     d prdeltcnt       s                   like(pr_deltcnt)
     d prspltprm       s                   like(pr_spltprm)
     d prsplttmp       s                   like(pr_splttmp)
     d prendper        s                   like(pr_endper)
     d prendpers       s                   like(pr_endpers)
     d prfbuydat       s                   like(pr_fbuydat)
     d prlstordr       s                   like(pr_lstordr)
     d prlstrcvd       s                   like(pr_lstrcvd)
     d prlstintr       s                   like(pr_lstintr)
     d praccsale       s                   like(pr_accsale)
     d praccouts       s                   like(pr_accouts)
     d praccdem        s                   like(pr_accdem)
     d prrnkbuy$       s                   like(pr_rnkbuy$)
     d prrnkbuyu       s                   like(pr_rnkbuyu)
     d prrnkloc$       s                   like(pr_rnkloc$)
     d prrnklocu       s                   like(pr_rnklocu)
     d prrnkcom$       s                   like(pr_rnkcom$)
     d prrnkcomu       s                   like(pr_rnkcomu)
     d prrnksup$       s                   like(pr_rnksup$)
     d prrnksupu       s                   like(pr_rnksupu)
     d prrnkdate       s                   like(pr_rnkdate)
     d prpoqtydv       s                   like(pr_poqtydv)
     d prpoqtyum       s                   like(pr_poqtyum)
     d prdeal          s                   like(pr_deal)
     d prdealbeg       s                   like(pr_dealbeg)
     d prdealend       s                   like(pr_dealend)
     d prdealalw       s                   like(pr_dealalw)
     d prdealuse       s                   like(pr_dealuse)
     d prprvfore       s                   like(pr_prvfore)
     d prprvdevp       s                   like(pr_prvdevp)
     d prprvserr       s                   like(pr_prvserr)
     d prprvts         s                   like(pr_prvts)
     d prprvlost       s                   like(pr_prvlost)
     d prprvsale       s                   like(pr_prvsale)
     d prprvexpt       s                   like(pr_prvexpt)
     d prprvdemd       s                   like(pr_prvdemd)
     d prtrnoord       s                   like(pr_trnoord)
     d praltoord       s                   like(pr_altoord)
     d prcontflg       s                   like(pr_contflg)
     d proverflg       s                   like(pr_overflg)
     d proverunt       s                   like(pr_overunt)
     d provercst       s                   like(pr_overcst)
     d provercur       s                   like(pr_overcur)
     d provermax       s                   like(pr_overmax)
     d prmfgout        s                   like(pr_mfgout)
     d prprocalt       s                   like(pr_procalt)
     d prformeth       s                   like(pr_formeth)
     d prinvmeth       s                   like(pr_invmeth)
     d prexcuntl       s                   like(pr_excuntl)
     d prexcaftr       s                   like(pr_excaftr)
     d prrebate        s                   like(pr_rebate)
     d prtranptd       s                   like(pr_tranptd)
     d prtranlst       s                   like(pr_tranlst)
     d prrepcary       s                   like(pr_repcary)
     d prexclead       s                   like(pr_exclead)
     d prnonstck       s                   like(pr_nonstck)
     d prcomprod       s                   like(pr_comprod)
     d prcmblocn       s                   like(pr_cmblocn)
     d prcmbsupl       s                   like(pr_cmbsupl)
     d prcmbsub        s                   like(pr_cmbsub)
     d pracctyp1       s                   like(pr_acctyp1)
     d pracctyp2       s                   like(pr_acctyp2)
     d pracctyp3       s                   like(pr_acctyp3)
     d pracctyp4       s                   like(pr_acctyp4)
     d pracctyp5       s                   like(pr_acctyp5)
     d pracctyp6       s                   like(pr_acctyp6)
     d pracctyp7       s                   like(pr_acctyp7)
     d pracctyp8       s                   like(pr_acctyp8)
     d pracctyp9       s                   like(pr_acctyp9)
     d prcarcoun       s                   like(pr_carcoun)
     d prusera1        s                   like(pr_usera1)
     d prusera2        s                   like(pr_usera2)
     d prusera3        s                   like(pr_usera3)
     d prusern1        s                   like(pr_usern1)
     d prusern2        s                   like(pr_usern2)
     d prusern3        s                   like(pr_usern3)
     d prdisotho       s                   like(pr_disotho)
     d prdisunt7       s                   like(pr_disunt7)
     d prdisun7o       s                   like(pr_disun7o)
     d prdisunt8       s                   like(pr_disunt8)
     d prdisun8o       s                   like(pr_disun8o)
     d prdisunt9       s                   like(pr_disunt9)
     d prdisun9o       s                   like(pr_disun9o)

     d total_12        s             15  0                                      total for perd 12
     d total_13        s             15  0                                      total for perd 13
     d total_52        s             15  0                                      total for perd 52
     d rolled_12       s             15  0                                      total rolled perd 12
     d rolled_13       s             15  0                                      total rolled perd 13
     d rolled_52       s             15  0                                      total rolled perd 52
     d ye_for_12       s              1
     d ye_for_13       s              1
     d ye_for_52       s              1

     d px_comp         s                   like(pr_comp)
     d px_locn         s                   like(pr_locn)
     d px_prod         s                   like(pr_prod)
     d px_supl         s                   like(pr_supl)
     d px_suplsub      s                   like(pr_suplsub)

     d sx_comp         s                   like(sp_comp)
     d sx_locn         s                   like(sp_locn)
     d sx_supl         s                   like(sp_supl)
     d sx_suplsub      s                   like(sp_suplsub)
     d saved_name      s                   like(sp_name)
     d saved_buyr      s                   like(sp_buyr)
     d saved_lt        s                   like(sp_leadtmo)
     d saved_ltv       s                   like(sp_leadtmv)

      * ------------------------------ work fields passed to module K3S_M080
     d fordevp         s              3  3                                      DEV %
     d nodevp          s              3  3                                      default DEV %
     d accdem          s                   like(pr_accdem)                      accumulated demand
     d forcast         s                   like(pr_forcast)                     forecast, average
     d forserr         s                   like(pr_forserr)                     smoothed error
     d forserr_AV      s                   like(pr_forserr)                     absolute value smoot

     d time_stam1      s               z   inz
     d time            s               t   timfmt(*iso)
     d date            s               d   datfmt(*iso)
     d #birth          s               d   datfmt(*iso)
     d #birthtm        s               t   timfmt(*iso)

     d convpkw1        s                   like(pr_qtyohnd)
     d convpkw2        s                   like(pr_qtyohnd)

     d transpl_cnt     s              5  0
     d tranprd_cnt     s              5  0
     d prodlog_cnt     s              5  0
     d product_cnt     s              5  0
     d prodfor_cnt     s              5  0

     d*ArraySize       s             10i 0 inz(150)
     d*intprod         ds                  qualified inz occurs(150)
     d*  ip_comp                      1
     d*  ip_locn                      5
     d*  ip_birth                      d
     d*  ip_lastupd                    d
     d*  ip_supl                     10
     d*  ip_suplsub                  10
     d*  ip_suplusr                  10
     d*  ip_suplusb                  10
     d*  ip_prod                     25
     d*  ip_prodseq                  40
     d*  ip_desc1                    40
     d*  ip_desc2                    40
     d*  ip_mfg                      25
     d*  ip_ndc_upc                  25
     d*  ip_uom                       4
     d*  ip_packsiz                  10
     d*  ip_tihi                     10
     d*  ip_status                    1
     d*  ip_minqty                    7  0
     d*  ip_buymult                   7  0
     d*  ip_group1                   10
     d*  ip_group2                   10
     d*  ip_group3                   10
     d*  ip_group4                   10
     d*  ip_group5                   10
     d*  ip_whslocn                  10
     d*  ip_costreg                  11  4
     d*  ip_costdiv                   5  0
     d*  ip_sales                    11  4
     d*  ip_salesw                   11  4
     d*  ip_qtyohnd                   7  0
     d*  ip_qtyoord                   7  0
     d*  ip_qtyback                   7  0
     d*  ip_weight                    7  3
     d*  ip_weightd                   5  0
     d*  ip_volume                    7  3
     d*  ip_volumed                   5  0
     d*  ip_dlysale                   7  0
     d*  ip_dlyouts                   7  0
     d*  ip_trnoord                   7  0
     d*  ip_altoord                   7  0
     d*  ip_forctyp                   1  0
     d*  ip_convpak                   7  0
     d*  ip_purincr                   5  0
     d*  ip_contflg                   1  0
     d*  ip_rebate                    3  1
     d*  ip_procalt                   1  0
     d*  ip_dlytyp1                   7  0
     d*  ip_dlytyp2                   7  0
     d*  ip_dlytyp3                   7  0
     d*  ip_dlytyp4                   7  0
     d*  ip_dlytyp5                   7  0
     d*  ip_dlytyp6                   7  0
     d*  ip_dlytyp7                   7  0
     d*  ip_dlytyp8                   7  0
     d*  ip_dlytyp9                   7  0
     d*  ip_carcoun                   1  0
     d*  ip_usera1                    1
     d*  ip_usera2                    3
     d*  ip_usera3                   10
     d*  ip_usern1                    5  0
     d*  ip_usern2                    7  2
     d*  ip_usern3                   11  4
     d*  ip_disothr                   9  4
     d*  ip_disunt7                   9  4
     d*  ip_disunt8                   9  4
     d*  ip_disunt9                   9  4

     d LgArySize       s             10i 0 inz(5)
     d logprod         ds                  qualified inz occurs(5)
     d   lg_comp                      1
     d   lg_locn                      5
     d   lg_buyr                      5
     d   lg_supl                     10
     d   lg_suplsub                  10
     d   lg_suplusr                  10
     d   lg_suplusb                  10
     d   lg_suplnam                  40
     d   lg_prod                     25
     d   lg_desc1                    40
     d   lg_logtype                   1
     d   lg_deltcnt                   1  0
     d   lg_timestp                    z
     d   lg_prvsupl                  10
     d   lg_prvsub                   10
     d   lg_prvsplu                  10
     d   lg_prvsplb                  10
     d   lg_prvname                  40
     d   lg_user                     10

      * -------------------------------------------------------
     d locatns_rec   e ds                  ExtName(k_locatns)
     d suplier_rec   e ds                  ExtName(k_suplier)
     d prodlog_rec   e ds                  ExtName(k_prodlog)
     d logprod_rec   e ds                  ExtName(k_logprod)
     d product_rec   e ds                  ExtName(k_product)
     d prodfor_rec   e ds                  ExtName(k_prodfor)
     d intprod_rec   e ds                  ExtName(k_intprod)

      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s           5000a   varying inz
     d String          s             40a   inz
     d InpSrchCnd      s           3000a   varying inz
      * -------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * -------------------------------------------------- procedure interface
     d K3S_9010        PI
     d  comp                          1
     d  per_end_12                    1                                         period end parm 12
     d  per_end_13                    1                                         period end parm 13
     d  per_end_52                    1                                         period end parm 52
     d  supl_chg                      1                                         supplier has changed
     d  today_str                    26                                         start timestamp
     d  today_end                    26                                         end timestamp
     d  get_log_5                     1                                         get logtype '5' data
     d  first_time                    1                                         1st time 'D' make 'M
     d  yearend_12                    1
     d  yearend_13                    1
     d  yearend_52                    1


      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;
       //
       // retrieve local data area *lda
       in *dtaara;

       // clear fields to total number of records processed for each log type
       clear lda_recprc;
       clear lda_rctyp1;
       clear lda_rctyp2;
       clear lda_rctyp3;
       clear lda_rctyp4;
       clear lda_rctyp5;
       clear lda_rctyp6;
       clear lda_rctyp7;
       clear lda_rctyp8;
       clear lda_rctyp9;
       clear lda_rctypx;

       // Read Products and increment counter for total # of records processed
       exsr dclprcursorc;
       exsr dclipcursor;
       exsr dcllccursor;
       exsr clsipcursor;

       exsr opnipcursor;

       // ---------------------------------------------------------- Main Loop
       // main loop
       dow SQLState = SQLStateOk;

       //fetch intprod row
           exec sql
             fetch next
               from ipcursor
               into :intprod_rec;

           if SQLState = RowNotFound;
              leave;
           endif;

           lda_recprc +=  1;      //Increment total records processed counter

           //------------------------------------------------------ Once Routine

       // once routine
           if #once <> 1;
               #once = 1;

       // call module to retrieve timestamp
               callp K3S_Retrieve_Timestamp(time_stamp);
               today_str = %char(time_stamp);

               exec sql
                 select cm_sysdate, cm_formeth, cm_invmeth
                   into :cmsysdate, :cmformeth, :cminvmeth
                   from k_company
                   where cm_comp = :ip_comp;
               if SQLState = SQLStateOk;
       // develop future date 90 days out
                  Ninety_Out = cmsysdate + %days(90);
               endif;

      // save alternate source blocking method
       //      block_alts = 1 means that user will maintain blocking via
       //                     Product maintenance K3S_3000
       //      block_alts = 0 means that user will maintain blocking via
       //                     interface file K_INTPROD
               exec sql
                  select ta_flag1
                    into :taflag1
                    from k_tablcod
                    where ta_comp = :ip_comp and
                          ta_codetyp = 'APP' and
                          ta_codeval = 'K3S_9010  BLOCK_ALTS'
                    fetch first row only;
                if SQLState = SQLStateOk;
                    block_alts = taflag1;
                else;
                    block_alts = 0;
                endif;

       // New to Slow for system status during product add
       //      new_slow = 1 means that this product will automatically
       //                   have system status changed from 'N' to 'S'
       //                   (New to Slow) during add to system
       //      new_slow = 0 no automatic change made to system status
       //                   when New product added to system
       //        new_usedft = 1 means that this product should use a default
       //                     average stored in field Number 2
               exec sql
                  select ta_flag1, ta_flag2, ta_number2
                  into :taflag1, :taflag2, :tanumber2
                  from k_tablcod
                  where ta_comp = :ip_comp and
                        ta_codetyp = 'APP' and
                        ta_codeval = 'K3S_9010  NEW_SLOW  '
                  fetch first row only;
               if SQLState = SQLStateOk;

                  new_slow = taflag1;
                  new_usedft = taflag2;
                  new_avgdft = tanumber2;
               else;
                  new_slow = 0;
                  new_usedft = 0;
                  new_avgdft = 0;
               endif;

       // delay days processing logic
       //      delay_days = 1 means that logic should be used
       //      delay_days = 0 means not using this logic
               exec sql
                 select ta_flag1, ta_number1
                   into :taflag1, :tanumber1
                   from k_tablcod
                   where ta_comp = :ip_comp and
                         ta_codetyp = 'APP' and
                         ta_codeval = 'DELAY_DAYSPROCESSING'
                   fetch first row only;
               if SQLState = SQLStateOk;
                   delay_days = taflag1;
                   delay_diff = tanumber1;
               else;
                   delay_days = 0;
                   delay_diff = 0;
               endif;

       // new product automatic deal copy logic
       //      copy_deals = 1 means that logic should be used
       //      copy_deals = 0 means not using this logic
               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_5056  PROCESSING'
                  fetch first row only;
               if SQLState = SQLStateOk;
                  copy_deals = taflag1;
               else;
                  copy_deals = 0;
               endif;

       // clear PR_EXCUNTL after expiration
       //      clr_record = 1 means that PR_EXCUNTL should be cleared
       //      clr_record = 0 means to not execute logic

               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'EXCL_BUY  CLR_RECORD'
                     fetch first row only;
               if SQLState = SQLStateOk;
                  clr_record = taflag1;
               else;
                  clr_record = 0;
               endif;

       // clear PR_PROMQTY, PR_PROMBEG, PR_PROMEND after expiration
       //      clr_promo  = 1 means that 3 fields should be cleared
       //      clr_promo  = 0 means to not execute logic
               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  CLR_PROMO '
                     fetch first row only;
               if SQLState = SQLStateOk;
                  clr_promo  = taflag1;
               else;
                  clr_promo  = 0;
               endif;

       // include back orders in 'count days out' logic?
       //      back_order = 1 means to include back orders in logic
       //      back_order = 0 means to not include back orders in logic

               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  DAYS_OUT  '
                     fetch first row only;
               if SQLState = SQLStateOk;
                  back_order = taflag1;
               else;
                  back_order = 0;
               endif;

       // include lost sales in 'count days out' logic?
       //      lost_sales = 1 means to include lost sales in logic
       //      lost_sales = 0 means to not include lost sales in logic
               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  DAYS_OUT_2'
                     fetch first row only;
               if SQLState = SQLStateOk;
                  lost_sales = taflag1;
               else;
                  lost_sales = 0;
               endif;

       // new products at Bindley, no deal copy for Location 06
       //      no_deal_06 = 1 means this is Bindley Western, perform test
       //      no_deal_06 = 0 means this is NOT Bindley Western

               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'BINDLEY   NO_DEAL_06'
                     fetch first row only;
               if SQLState = SQLStateOk;
                  no_deal_06 = taflag1;
               else;
                  no_deal_06 = 0;
               endif;

       // look for Minimum Seasonal Factor being used
       //    must be reasonable value between .01 and .50
       //    or special logic will not be used

               min_used   = 0;
               min_season = 0;
               exec sql
                  select ta_flag1, ta_number2
                     into :taflag1, :tanumber2
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_M080  MIN_SEASON'
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag1 = 1;
                   min_season = tanumber2;
                  if min_season < 0 or min_season > .50;
                     min_season = 0;
                  endif;
               else;
                  min_season = 0;
               endif;
               if min_season > 0;
                  min_used   = 1;
               endif;

       // determine if customer wants 'cost sent as $0' logged
       //   into K_LOGPROD file as type 'c'. If the answer is yes,
       //   then see if the customer wants to use a default value
       //   for cost, stored in Number2 field.

               cost_0     = 0;
               cost_fill  = 0;
               cost_value = 0;
               exec sql
                  select ta_flag1, ta_flag2, ta_number2
                     into :taflag1, :taflag2, :tanumber2
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  COST_0    '
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag1 = 1;
                  cost_0 = 1;
                  if taflag2 = 1 AND tanumber2 > 0;
                     cost_fill = 1;
                     cost_value = tanumber2;
                  endif;
               endif;

       //  determine if customer wants field PR_LSTRCVD updated
       //    daily in this program, which will be used in program K3S_3600
       //    to filter any products not received yet from Overstock Report

               lstrcvd    = 0;
               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  PR_LSTRCVD'
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag1 = 1;
                  lstrcvd    = 1;
                  days_10out = cmsysdate + %days(10);
               endif;

       // determine if customer wants field PR_RFBIRTH updated
       //   daily in this program, which will be used in program K3S_3600
       //   to filter any products with Reference Birth logic in play
       //      ***  notice that flag 2 is used to control the population
       //      ***  of PR_RFBIRTH

               ref_birth  = 0;
               exec sql
                  select ta_flag2
                     into :taflag2
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_3600  PR_RFBIRTH'
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag2 = 1;
                   ref_birth  = 1;
               endif;

       // determine if customer wants field PR_BIRTH populated
       //   using IP_BIRTH for new products.

               NewBirth = 0;
               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  PR_BIRTH  '
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag1 = 1;
                  NewBirth = 1;
               endif;

       // for customers who use Corporate Profiles
       //   corp_prof = 1 means customer does use Corporate Profiles
       //   corp_prof = 0 means customer does not use Corporate Profiles

               corp_prof  = 0;
               exec sql
                  select ta_flag1, ta_codeds3
                     into :taflag1, :tacodeds3
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  CORP_PROF '
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag1 = 1;
                  corp_prof  = 1;
                  CorpProfID = tacodeds3;
               endif;
       // for Multi-location customers who can provide transfer info
       //   UpdQtyPend= 1 customer passing transfer info via IP_TRNORD
       //   UpdQtyPend= 0 customer not passing transfer info
               UpdQtyPend = 0;
               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  PR_QTYPEND'
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag1 = 1;
                  UpdQtyPend = 1;
               endif;

       // will Car Count be updated from interface
       //   car_update = 1 means car count updated from interface file
       //   car_update = 0 means car count not updated from interface
               car_update = 0;
               exec sql
                  select ta_flag2
                     into :taflag2
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_1500  CAR_COUNT '
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag2 = 1;
                  car_update = 1;
               endif;

       // PE1 filtering in play?
       //   PE1_FLTR_1 = 1 means we do want to filter PE1 when Demand <= Man Min Unit
       //   PE1_FLTR_1 = 0 means we do not want to filter PE1 checks
               PE1_FLTR_1 = 0;
               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                           ta_codeval = 'K3S_9010  PE1_FLTR_1'
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag1 = 1;
                  PE1_FLTR_1 = 1;
               endif;

       // PE snapshots in play?
       //   pe_snpshot = 1 means we do want to collect period ending snapshots
       //   pe_snpshot = 0 means we do not want to collect pe snapshots
               pe_snpshot = 0;
               exec sql
                  select ta_flag1
                     into :taflag1
                     from k_tablcod
                     where ta_comp = :ip_comp and
                           ta_codetyp = 'APP' and
                          ta_codeval = 'K3S_9010  PE_SNPSHOT'
                     fetch first row only;
               if SQLState = SQLStateOk AND taflag1 = 1;
                  pe_snpshot = 1;
               endif;

       // the purpose of this section is to read the very first
       //   location record, to ensure that the field lc_buyrbad is available
       //   to be used if a lg_logtype of '6' is generated for
       //   an invalid location being passed.
       //    exsr dcllccursor;
               exsr opnlccursor;
               if SQLState = SQLStateOk;
                  exec sql
                     fetch next
                       from lccursor
                      into :locatns_rec;
               endif;
               exsr clslccursor;
           endif;


       // ----------------------------------------------------- Location break
           if ip_locn <> locn;
              locn = ip_locn;

       //  get location
               exec sql
                 select *
                   into :locatns_rec
                   from k_locatns
                   where lc_comp = :ip_comp and
                         lc_locn = :ip_locn
                   fetch first row only;

       //    test if location set up in database
               if SQLState = SQLStateOk;
                  bad_locn  = *off;

       //      make demand filter low limit a negative value
                  demlo = lc_demlo * -1;

       //       save answer for later testing in Delay Days processing
                  if delay_days = 1;
                     delay_test = lc_sysdate - %days(delay_diff);
                  endif;

               else;
                  bad_locn  = *on;

               endif;

           endif;

       // ----------------------------------------------------- Supplier break

      // change in supplier ID

           #iplocn = ip_locn;
           #ipsupl = ip_supl;
           #ipsuplsub = ip_suplsub;

           if supl <> suplsaved;

       // save new supplier ID
              suplsaved = supl;

       // get supplier record
              exec sql
                select *
                  into :suplier_rec
                  from k_suplier
                  where sp_comp = :ip_comp and
                        sp_locn = :ip_locn and
                        sp_supl = :ip_supl and
                        sp_suplsub = :ip_suplsub
                  fetch first row only;

       //     test if supplier set up in database
              if SQLState = SQLStateOk;
                 bad_suplr  = *off;
                 saved_name = sp_name;
                 saved_buyr = sp_buyr;
                 saved_lt   = sp_leadtmo;
                 saved_ltv  = sp_leadtmv;
              else;
                 bad_suplr = *on;
              endif;

           endif;
      // ---------------------------------- Process product interface records
      //     always start Log Type as blank, no carry over from
       //     previous product

           lg_logtype = ' ';
       // If location no good, then log error, and by-pass the rest of tests
           pr_deltcnt = 0;
           if bad_locn  = *on;
              lg_logtype = '6';
              lg_prvsupl = *blanks;
              lg_prvsub  = *blanks;
              lg_prvsplu = *blanks;
              lg_prvsplb = *blanks;
              lg_prvname = *blanks;
              // sp_buyr    = 'ZZZ';
              sp_buyr    = lc_buyrbad;
              sp_name    = bad_locnd;
              exsr insert_logprod;

       // Location is fine, so continue
           else;

       // If supplier no good, then log error, and by-pass the rest of tests
              if bad_suplr = *on;
                 lg_logtype = '5';
                 lg_prvsupl = *blanks;
                 lg_prvsub  = *blanks;
                 lg_prvsplu = *blanks;
                 lg_prvsplb = *blanks;
                 lg_prvname = *blanks;
                 sp_buyr    = lc_buyrbad;
                 sp_name    = bad_suplrd;
                 exsr insert_logprod;

       // Supplier is fine, so continue
              else;

       // This next set of tests will determine if product is already in
       //   the database, and can simply be updated with new information.
       //   The product may exist under a different supplier id. Depending
       //   on the type of industry being processed, a decision is made to
       //   either create a new product record, or simply to copy existing
       //   product information that exists under a different supplier to
       //   the new supplier.

                 exist_here = *off;
                 exist_othr = *off;

                 exec SQL
                    select pr_comp, pr_buyr, pr_regn, pr_locn, pr_supl,
                           pr_suplsub, pr_suplusr, pr_suplusb,
                           pr_suplorg, pr_suplors, pr_prod, pr_altsrce,
                           pr_tempory, pr_prodseq, pr_desc1, pr_desc2,
                           pr_ndc_upc, pr_mfg, pr_catalog, pr_uom,
                           pr_packsiz, pr_tihi, pr_group1, pr_group1o,
                           pr_group2, pr_group2o, pr_group3, pr_group3o,
                           pr_group4, pr_group4o, pr_group5, pr_group5o,
                           pr_whslocn, pr_birth, pr_rfbirth, pr_lastupd,
                           pr_linecst, pr_leadtm, pr_leadtmp, pr_leadtmv,
                           pr_leadtms, pr_leadtmt, pr_costreg,
                           pr_costdiv, pr_costeac, pr_ovrcreg, pr_ovrcdiv,
                           pr_costlst, pr_costldt, pr_brkflag,
                           pr_brkcost, pr_sales, pr_saleslw, pr_qtyohnd,
                           pr_qtyoord, pr_qtyback, pr_qtyhold,
                           pr_qtypend, pr_qtybaln, pr_promqty,
                           pr_prombeg, pr_promend, pr_usrstat, pr_sysstat,
                           pr_soqnite, pr_forcast, pr_formanl,
                           pr_custusg, pr_forfrez, pr_probdat, pr_fordevp,
                           pr_forserr, pr_forchg,
                           pr_minqty, pr_minqtyo, pr_buymult, pr_buymulo,
                           pr_buymuli, pr_convpak, pr_convpko, pr_convpkp,
                           pr_daysout, pr_daysprv, pr_daysunt, pr_intrval,
                           pr_otimfac, pr_maxdays, pr_maminid, pr_mamaxid,
                           pr_maminiu, pr_mamaxiu,
                           pr_add_day, pr_ansale$, pr_ansaleu, pr_devtimf,
                           pr_sstimef, pr_safesum, pr_weight, pr_weighto,
                           pr_weightd, pr_weighdo, pr_volume, pr_volumeo,
                           pr_volumed, pr_volumdo, pr_purincr, pr_purinco,
                           pr_disothr, pr_seasonl, pr_seasact, pr_seassrc,
                           pr_service, pr_servsim, pr_svceprv, pr_forcint,
                           pr_forcyr, pr_forcper, pr_fstslyr, pr_fstslpr,
                           pr_histper, pr_promper, pr_longtrm, pr_deltcnt,
                           pr_spltprm, pr_splttmp, pr_endper,pr_endpers,
                           pr_fbuydat, pr_lstordr, pr_lstrcvd, pr_lstintr,
                           pr_accsale, pr_accouts, pr_accdem, pr_rnkbuy$,
                           pr_rnkbuyu, pr_rnkloc$, pr_rnklocu, pr_rnkcom$,
                           pr_rnkcomu, pr_rnksup$, pr_rnksupu, pr_rnkdate,
                           pr_poqtydv, pr_poqtyum, pr_deal, pr_dealbeg,
                           pr_dealend, pr_dealalw, pr_dealuse, pr_prvfore,
                           pr_prvdevp, pr_prvserr, pr_prvts, pr_prvlost,
                           pr_prvsale, pr_prvexpt, pr_prvdemd, pr_trnoord,
                           pr_altoord, pr_contflg, pr_overflg, pr_overunt,
                           pr_overcst, pr_overcur, pr_overmax, pr_mfgout,
                           pr_procalt, pr_formeth, pr_invmeth, pr_excuntl,
                           pr_excaftr, pr_rebate, pr_tranptd, pr_tranlst,
                           pr_repcary, pr_exclead, pr_nonstck, pr_comprod,
                           pr_cmblocn, pr_cmbsupl, pr_cmbsub, pr_acctyp1,
                           pr_acctyp2, pr_acctyp3, pr_acctyp4, pr_acctyp5,
                           pr_acctyp6, pr_acctyp7, pr_acctyp8, pr_acctyp9,
                           pr_carcoun, pr_usera1, pr_usera2, pr_usera3,
                           pr_usern1, pr_usern2, pr_usern3, pr_disotho,
                           pr_disunt7, pr_disun7o, pr_disunt8,
                           pr_disun8o, pr_disunt9, pr_disun9o

                    into :product_rec
                      //   :prcomp, :prbuyr, :prregn, :prlocn, :prsupl,
                      //   :prsuplsub, :prprod, :prsuplusr, :prsuplusb,
                      //   :prsuplorg, :prsuplors, :praltsrce,
                      //   :prtempory, :prprodseq, :prdesc1, :prdesc2,
                      //   :prndc_upc, :prmfg, :prcatalog, :pruom,
                      //   :prpacksiz, :prtihi, :prgroup1, :prgroup1o,
                      //   :prgroup2, :prgroup2o, :prgroup3, :prgroup3o,
                      //   :prgroup4, :prgroup4o, :prgroup5, :prgroup5o,
                      //   :prwhslocn, :prbirth, :prrfbirth, :prlastupd,
                      //   :prlinecst, :prleadtm, :prleadtmp, :prleadtmv,
                      //   :prleadtms, :prleadtmt, :prcostreg,
                      //   :prcostdiv, :prcosteac, :provrcreg,:provrcdiv,
                      //   :prcostlst, :prcostldt, :prbrkflag,
                      //   :prbrkcost, :prsales, :prsaleslw, :prqtyohnd,
                      //   :prqtyoord, :prqtyback, :prqtyhold,
                      //   :prqtypend, :prqtybaln, :prpromqty,
                      //   :prprombeg, :prpromend, :prusrstat,:prsysstat,
                      //   :prsoqnite, :prforcast, :prformanl,
                      //   :prcustusg, :prforfrez, :prprobdat,:prfordevp,
                      //   :prforserr, :prforchg,
                      //   :prminqty, :prminqtyo, :prbuymult, :prbuymulo,
                      //   :prbuymuli, :prconvpak, :prconvpko,:prconvpkp,
                      //   :prdaysout, :prdaysprv, :prdaysunt,:printrval,
                      //   :protimfac, :prmaxdays, :prmaminid,:prmamaxid,
                      //   :prmaminiu, :prmamaxiu,
                      //   :pradd_day, :pransale$, :pransaleu,:prdevtimf,
                      //   :prsstimef, :prsafesum, :prweight, :prweighto,
                      //   :prweightd, :prweighdo, :prvolume, :prvolumeo,
                      //   :prvolumed, :prvolumdo, :prpurincr,:prpurinco,
                      //   :prdisothr, :prseasonl, :prseasact,:prseassrc,
                      //   :prservice, :prservsim, :prsvceprv,:prforcint,
                      //   :prforcyr, :prforcper, :prfstslyr, :prfstslpr,
                      //   :prhistper, :prpromper, :prlongtrm,:prdeltcnt,
                      //   :prspltprm, :prsplttmp, :prendper, :prendpers,
                      //   :prfbuydat, :prlstordr, :prlstrcvd,:prlstintr,
                      //   :praccsale, :praccouts, :praccdem, :prrnkbuy$,
                      //   :prrnkbuyu, :prrnkloc$, :prrnklocu,:prrnkcom$,
                      //   :prrnkcomu, :prrnksup$, :prrnksupu,:prrnkdate,
                      //   :prpoqtydv, :prpoqtyum, :prdeal, :prdealbeg,
                      //   :prdealend, :prdealalw, :prdealuse,:prprvfore,
                      //   :prprvdevp, :prprvserr, :prprvts, :prprvlost,
                      //   :prprvsale, :prprvexpt, :prprvdemd,:prtrnoord,
                      //   :praltoord, :prcontflg, :proverflg,:proverunt,
                      //   :provercst, :provercur, :provermax, :prmfgout,
                      //   :prprocalt, :prformeth, :prinvmeth,:prexcuntl,
                      //   :prexcaftr, :prrebate, :prtranptd, :prtranlst,
                      //   :prrepcary, :prexclead, :prnonstck,:prcomprod,
                      //   :prcmblocn, :prcmbsupl, :prcmbsub, :pracctyp1,
                      //   :pracctyp2, :pracctyp3, :pracctyp4,:pracctyp5,
                      //   :pracctyp6, :pracctyp7, :pracctyp8,:pracctyp9,
                      //   :prcarcoun, :prusera1, :prusera2, :prusera3,
                      //   :prusern1, :prusern2, :prusern3, :pr_disotho,
                      //   :prdisunt7, :prdisun7o, :prdisunt8,
                      //   :prdisun8o, :prdisunt9, :prdisun9o
                         from k_product
                         where pr_comp = :ip_comp and
                               pr_locn = :ip_locn and
                               pr_supl = :ip_supl and
                               pr_suplsub = :ip_suplsub and
                               pr_prod = :ip_prod
                         fetch first row only;

       //      product does exist at this supplier
                 if SQLState = SQLStateOk;
                    exist_here = *on;

       //      if product had been in delete count mode, then it is
       //         being re-instated, so send a record to the log file
                    if pr_deltcnt > 0;
                       lg_logtype = '7';
                       lg_prvsupl = *blanks;
                       lg_prvsub  = *blanks;
                       lg_prvsplu = *blanks;
                       lg_prvsplb = *blanks;
                       lg_prvname = *blanks;

                       exsr insert_logprod;
                    endif;
                 else;

       //   look for product record at this location
                    px_comp    = ip_comp;
                    px_locn    = ip_locn;
                    px_prod    = ip_prod;

       //      does product exist at another supplier ?
                    exec sql
                      select count(*)
                        into :product_cnt
                        from k_product
                        where pr_comp = :px_comp and
                              pr_locn = :px_locn and
                              pr_prod = :px_prod;
       //      if yes, then see if it is regular source product
                    if product_cnt > 0;

                       exsr InzInpSrchPrdC;

                       exsr IntSQLStmt;

                       exsr PrepDynSQLStmtC;

                       if SQLState = SQLStateOk;

                          exsr opnprcursorc;

                          Dow SQLState = SQLStateOk;
                             exec sql
                                fetch next
                                   from prcursorc
                                   into :product_rec;

                             if SQLState = RowNotFound;
                                leave;
                             endif;

                             if pr_altsrce = 0;

                                exist_othr = *on;
                                px_supl    = pr_supl;
                                px_suplsub = pr_suplsub;
                                leave;
                             endif;
                          enddo;
                          exsr clsprcursorc;
                       endif;
                    endif;
                 endif;
       //Product is allowed to be switched to another supplier
                 if lc_swallow = 1;

                    select;

       //   product does exist at this supplier, so just update
                       when exist_here = *on;
                            exsr   $_update;
                            exsr   update_producta;

       //   product does exist at another supplier, and supplier switching
       //       allowed, so move it.
                       when exist_othr = *on;
                            exsr $_switch;

       //   product does not exist at another supplier, so add it
                       when exist_othr = *off;
                            exsr  $_newprod;
                            exsr  $_update;
                            exsr  insert_product;

                            exsr  $_prodlog;

       //   go check for deals
                            if copy_deals = 1;
       //   but make sure new product NOT under Hub Transfer Supplier
                               trans_prod = 0;
                               exsr $_transfer;
                               if trans_prod = 0;
       //   if Bindley, make sure its not location 06
                                  if no_deal_06 = 0 OR
                                    (no_deal_06 = 1 AND pr_locn <> '06');
                                    night = 'Y';
                                    callp K3S_5056(pr_comp:
                                                   pr_locn:
                                                   pr_prod:
                                                   pr_supl:
                                                   pr_suplsub:
                                                   night);
                                  endif;
                               endif;
                            endif;

                    endsl;

       //   product is not allowed to be switched to another supplier
                 else;

                    select;

       //   product does exist at this supplier, so just update
                       when exist_here = *on;
                            exsr   $_update;
                            exsr   update_producta;

       //   product does not exist at this supplier, so add it
                       when exist_here = *off;
                            exsr   $_newprod;
                            exsr   $_update;
                            exsr   insert_product;

                            exsr   $_prodlog;

       //   go check for deals
                            if copy_deals = 1;
       //   but make sure new product NOT under Hub Transfer Supplier
                               trans_prod = 0;
                               exsr $_transfer;
                               if trans_prod = 0;
       //   if Bindley, make sure its not location 06
                                  if no_deal_06 = 0 OR
                                     (no_deal_06 = 1 AND pr_locn <> '06');
                                      night = 'Y';
                                      callp K3S_5056(pr_comp:
                                                     pr_locn:
                                                     pr_prod:
                                                     pr_supl:
                                                     pr_suplsub:
                                                     night);
                                  endif;
                               endif;
                            endif;

                    endsl;

       //   lc_swallow
                 endif;
       //   bad_suplr
              endif;

       //   bad_locn
           endif;
       // read product interface records end of loop
           SQLState= SQLStateOk;
       enddo;

       // --------------------------------------------------- End of Main Loop

       // --------------------------------- determine if period end took place
       //     periodicity 12
       if total_12 > 0 and
          rolled_12 / total_12 > .50;

          per_end_12 = '1';
          if ye_for_12  = '1';
             yearend_12 = '1';
          endif;
       endif;

       //     periodicity 13
       if total_13 > 0 and
          rolled_13 / total_13 > .50;

          per_end_13 = '1';
          if ye_for_13  = '1';
             yearend_13 = '1';
          endif;
       endif;

       //     periodicity 52
       if total_52 > 0 and
          rolled_52 / total_52 > .50;

          per_end_52 = '1';
          if ye_for_52  = '1';
             yearend_52 = '1';
          endif;
       endif;

       //       update local data area *lda
       out *dtaara;

       // call module to retrieve timestamp
       callp K3S_Retrieve_Timestamp(time_stamp);
       today_end = %char(time_stamp);

       // finished, set on LR
       *inlr = *on;

       //***************************************************** End of program

       /////////////////////////////////////////////////////// Update product

       // update product fields from interface

       begsr $_update;

       //  initialize fields
       clear pr_soqnite;
       clear pr_qtybaln;
       clear pr_overflg;
       clear pr_overunt;
       clear pr_overcst;
       clear pr_overcur;
       clear pr_overmax;
       clear pr_qtypend;

       //  update buy group since this product could have switched buyers
       pr_buyr    = saved_buyr;

       //  keep original supplier up to date
       pr_suplorg = ip_supl;
       pr_suplors = ip_suplsub;

       pr_suplusr = ip_suplusr;
       pr_suplusb = ip_suplusb;

       //  last interface date
       pr_lstintr = lc_sysdate;

       // last update date
       pr_lastupd = lc_sysdate;

       //  region update
       pr_regn    = sp_regn;

       //  manufactor out flag
       pr_mfgout  = ip_mfgout;

       //  product sequence
       pr_prodseq = ip_prodseq;

       //  product description information

       //      product must have first description
       if ip_desc1 = *blanks;
          lg_logtype = 'K';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
          ip_desc1   = '** Missing description **';
          exsr insert_logprod;
       endif;
       pr_desc1   = ip_desc1;

       //     only update description 2 if data passed from host system
       if ip_desc2 <> *blanks;
          pr_desc2 = ip_desc2;
       endif;

       //     only update manufactor if data passed from host system
       if ip_mfg <> *blanks;
          pr_mfg = ip_mfg;
       endif;

       //      default catalog with ndc/upc code
       pr_catalog = ip_ndc_upc;

       //      only update ndc/upc code if data passed from host system
       if ip_ndc_upc <> *blanks;
          pr_ndc_upc = ip_ndc_upc;
       endif;

      //       only update unit of measure if data passed from host system
       if ip_uom <> *blanks;
          pr_uom = ip_uom;
       endif;

       //      only update pack size if data passed from host system
       if ip_packsiz <> *blanks;
          pr_packsiz = ip_packsiz;
       endif;

       //      only update ti/hi if data passed from host system
       if ip_tihi <> *blanks;
          pr_tihi = ip_tihi;
       endif;

       //      only update warehouse location if data passed from host system
       if ip_whslocn <> *blanks;
          pr_whslocn = ip_whslocn;
       endif;

       //  product status
       //      product status passed must be an 'R' or 'D'
       //        if not, then an 'R' is assumed
       if ip_status <> 'R' AND ip_status <> 'D';
          lg_logtype = 'V';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // if ip_status <> *blanks;
       //    ip_desc1 = 'Invalid status passed as ' +
       //               ip_status;
       // else;
       //    ip_desc1 = 'Invalid status passed as blank';
       // endif;
          exsr insert_logprod;
          ip_status = 'R';
       endif;

       //      if discontinued status passed, and product currently not
       //      set as discontinued, then change to discontinued status
       if ip_status  = 'D' AND pr_sysstat <> 'D';
          pr_sysstat = 'D';

       //          if the first time flag is on, then this customer wants
       //          any products going to 'D' for the first time, to
       //          automatically be changed to user status 'M'
       //            log these as type '8'
       //            clear any 'F' Freeze or 'P' Probation dates
          if first_time = '1';
             pr_usrstat = 'M';
             clear pr_forfrez;
             clear pr_probdat;
             lg_logtype = '8';
             lg_prvsupl = *blanks;
             lg_prvsub  = *blanks;
             lg_prvsplu = *blanks;
             lg_prvsplb = *blanks;
             lg_prvname = *blanks;
             sp_name    = saved_name;
             sp_buyr    = saved_buyr;
             exsr insert_logprod;
          endif;

       endif;

       //      if regular status passed, and product currently set as
       //      discontinued status, then re-instate product back to
       //      either 'R', 'L', or 'S'
       if ip_status  = 'R' AND pr_sysstat = 'D';
       //      first you must force system status to an 'R' for k3s_c160
          pr_sysstat = 'R';
       //------------------------------------------------ update system status
      /copy k3s_c160
       endif;

       //  group 1 logic
       pr_group1  = ip_group1;

       if exist_here = *off;
          pr_group1o = *blanks;
          pr_group2o = *blanks;
          pr_group3o = *blanks;
          pr_group4o = *blanks;
          pr_group5o = *blanks;
       endif;

       if pr_group1o <> *blanks and
          lc_prdgrp1 = 0 and
          exist_here = *on;

          pr_group1o = *blanks;
       endif;

       if pr_group1o <> *blanks;
          pr_group1  = pr_group1o;
       endif;

       //  group 2 logic
       pr_group2  = ip_group2;

       if pr_group2o <> *blanks and
          lc_prdgrp2 = 0 and
          exist_here = *on;

          pr_group2o = *blanks;
       endif;

       if pr_group2o <> *blanks;
          pr_group2  = pr_group2o;
       endif;


       //  group 3 logic
       pr_group3  = ip_group3;

       if pr_group3o <> *blanks and
          lc_prdgrp3 = 0 and
          exist_here = *on;

          pr_group3o = *blanks;
       endif;

       if pr_group3o <> *blanks;
          pr_group3  = pr_group3o;
       endif;


       //  group 4 logic
       pr_group4  = ip_group4;

       if pr_group4o <> *blanks and
          lc_prdgrp4 = 0 and
          exist_here = *on;

          pr_group4o = *blanks;
       endif;

       if pr_group4o <> *blanks;
          pr_group4  = pr_group4o;
       endif;

       //  group 5 logic
       pr_group5  = ip_group5;

       if pr_group5o <> *blanks and
          lc_prdgrp5 = 0 and
          exist_here = *on;

          pr_group5o = *blanks;
       endif;

       if pr_group5o <> *blanks;
          pr_group5  = pr_group5o;
       endif;

       //  pricing logic

       //      don't allow purchase price divisor to be less than 1
       if ip_costdiv < 1;
          lg_logtype = 'L';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Purchase price divisor ------>';
       // ip_costdiv = ip_costdiv * -1;
       // evalr ip_desc1 = %editc(ip_costdiv:'X');
          exsr insert_logprod;
          ip_costdiv = 1;
       endif;

       //      don't allow purchase price to be negative
       if ip_costreg < 0;
          lg_logtype = 'I';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative purchase price of--->';
       // evalr ip_desc1 = %editc(ip_costreg:'X');
       // ip_costreg = ip_costreg * -1;
          exsr insert_logprod;
          ip_costreg = 0;
       endif;

       //      Log if purchase price comes across as $0?
       //        Optionally determine if a default value to be used.
       //        If used it will be placed into purchase AND sales price.
       if ip_costreg = 0 and cost_0 = 1;
          lg_logtype = 'c';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
          if cost_fill  = 1;
             ip_costreg = cost_value;
             ip_sales   = cost_value;
          endif;
          exsr insert_logprod;
       endif;

       //      don't allow sales price to be negative
       if ip_sales   < 0;
          lg_logtype = 'J';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;

          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative sales price of ----->';
       // ip_sales   = ip_sales   * -1;
       // move      ip_sales      ip_desc1
          exsr insert_logprod;
          ip_sales   = 0;
       endif;

       //      don't allow sales price to be < cost
       if ip_sales   < ip_costreg / ip_costdiv;
          lg_logtype = 'Y';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks
       // ip_desc1   = 'Sales price < cost ---------->';
          ip_sales   = ip_costreg / ip_costdiv;
       // move      ip_sales      ip_desc1
          exsr insert_logprod;
       endif;

       //    save cost changes
       if exist_here = *off;
          pr_ovrcreg = 0;
          pr_ovrcdiv = 1;
       endif;

       if pr_ovrcreg > 0 and
          lc_overcst = 0 and
          exist_here = *on;

          pr_ovrcreg = 0;
          pr_ovrcdiv = 1;
       endif;

       if pr_ovrcreg > 0 and
          lc_overcst = 1 and
          exist_here = *on;

          ip_costreg = pr_ovrcreg;
          ip_costdiv = pr_ovrcdiv;
       endif;

       if pr_costreg <> ip_costreg;
          pr_costlst = pr_costreg;
          pr_costldt = lc_sysdate;
       endif;

       //    make sure over-ride cost divisor is at least 1
       if pr_ovrcdiv < 1;
          pr_ovrcdiv = 1;
       endif;

       //    update costs
       pr_costreg = ip_costreg;
       pr_costdiv = ip_costdiv;
       pr_costeac = ip_costreg / ip_costdiv;

       //    update sales price
       pr_sales   = ip_sales;

       //    quantity logic

       //    on hand section
       if ip_qtyohnd < 0;
          lg_logtype = 'F';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks
       // ip_desc1   = 'Negative on hand of --->';
       // ip_qtyohnd = ip_qtyohnd * -1;
       // move      ip_qtyohnd    ip_desc1
          exsr insert_logprod;
          clear ip_qtyohnd;
       endif;

       //      is delay days logic being used ?
       //        if so, then test if yesterday's onhand has gone to 0,
       //        because of daily sales
       if delay_days = 1;
          if pr_sysstat <> 'D' AND
             pr_usrstat <> 'M' AND
             pr_ansaleu  > 0;

             if pr_qtyohnd > 0 AND
                ip_qtyohnd = 0 AND
                pr_birth   < delay_test AND
                ip_dlysale > 0;

                clear delayuntil;
                callp K3S_3860(ip_comp:
                               ip_locn:
                               ip_supl:
                               ip_suplsub:
                               sp_buyr:
                               pr_ansaleu:
                               lc_sysdate:
                               delayuntil);

                if delayuntil  > pr_excuntl;
                   pr_excuntl  = delayuntil;
                endif;
             endif;
          endif;
       endif;

       pr_qtyohnd = ip_qtyohnd;

       //     on order section
       if ip_qtyoord < 0;
          lg_logtype = 'G';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative on order of --->';
       // ip_qtyoord = ip_qtyoord * -1;
       // move      ip_qtyoord    ip_desc1
          exsr insert_logprod;
          clear ip_qtyoord;
       endif;
       pr_qtyoord = ip_qtyoord;

       //     back order section
       if ip_qtyback < 0;
          lg_logtype = 'H';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
        //ip_desc1   = *blanks
        //ip_desc1   = 'Negative back orders of --->'
        //ip_qtyback = ip_qtyback * -1;
        //move      ip_qtyback    ip_desc1
          exsr insert_logprod;
          clear ip_qtyback;
       endif;
       pr_qtyback = ip_qtyback;

       //  buying multiple
       //      is buying multiple being passed less than 1?
       if ip_buymult < 1;
          lg_logtype = 'M';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Invalid buying multiple passed';
          exsr insert_logprod;
          ip_buymult = 1;
       endif;
       pr_buymuli = ip_buymult;
       pr_buymult = ip_buymult;
       if pr_buymulo <> *zeros;
          pr_buymult = pr_buymulo;
       endif;

       //  minimum quantity
       //      is minimum quantity being passed less than 1?
       if ip_minqty < 1;
          lg_logtype = 'N';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Invalid min. quantity passed';
          exsr insert_logprod;
          ip_minqty  = 1;
       endif;
       pr_minqty  = ip_minqty;
       if pr_minqtyo <> *zeros;
          pr_minqty  = pr_minqtyo;
       endif;

       //  purchase increment
       //      is purchase increment being passed less than 0?
       if ip_purincr < 0;
          lg_logtype = 'O';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative purchase increment';
          exsr insert_logprod;
          ip_purincr = 0;
       endif;
       pr_purincr = ip_purincr;
       if pr_purinco <> *zeros;
          pr_purincr = pr_purinco;
       endif;

       //  Unit 6 bracket
       //      is unit 6 bracket value being passed less than 0?
       if ip_disothr < 0;
          lg_logtype = 'd';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
          exsr insert_logprod;
          ip_disothr = 0;
       endif;
       pr_disothr = ip_disothr;
       if pr_disotho <> *zeros;
          pr_disothr = pr_disotho;
       endif;

       //  Unit 7 bracket
       //      is unit 7 bracket value being passed less than 0?
       if ip_disunt7 < 0;
          lg_logtype = 'e';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
          exsr insert_logprod;
          ip_disunt7 = 0;
       endif;
       pr_disunt7 = ip_disunt7;
       if pr_disun7o <> *zeros;
          pr_disunt7 = pr_disun7o;
       endif;

       //  Unit 8 bracket
       //      is unit 8 bracket value being passed less than 0?
       if ip_disunt8 < 0;
          lg_logtype = 'f';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
          exsr insert_logprod;
          ip_disunt8 = 0;
       endif;
       pr_disunt8 = ip_disunt8;
       if pr_disun8o <> *zeros;
          pr_disunt8 = pr_disun8o;
       endif;

       //  Unit 9 bracket
       //      is unit 9 bracket value being passed less than 0?
       if ip_disunt9 < 0;
          lg_logtype = 'g';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
          exsr insert_logprod;
          ip_disunt9 = 0;
       endif;
       pr_disunt9 = ip_disunt9;
       if pr_disun9o <> *zeros;
          pr_disunt9 = pr_disun9o;
       endif;

       //  product rebate
       //      is rebate being passed less than 0?
       if ip_rebate  < 0;
       // lg_logtype = 'P';
          lg_logtype = 'Z';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative rebate passed';
          exsr insert_logprod;
          ip_rebate  = 0;
       endif;
       pr_rebate  = ip_rebate;

       //  convenience pack
       //      is convenience pack being passed less than 0?
       if ip_convpak  < 0;
          lg_logtype = 'Q';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks
       // ip_desc1   = 'Neg. convenience pack passed';
          exsr insert_logprod;
          ip_convpak = 0;
       endif;
       pr_convpak = ip_convpak;
       if pr_convpko <> *zeros;
          pr_convpak = pr_convpko;
       endif;

       //  convenience pack less than buy multiple
       //      is convenience pack being used, and if so is it less
       //      than the buy multiple used to build orders?
       if (pr_convpak > 0 AND
          pr_convpak < pr_buymult);

          lg_logtype = 'a';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Conv Pack less than Buy Mult';
          exsr insert_logprod;
       endif;

       //  convenience pack not a true multiple
       //      is convenience pack being used, and if so is it evenly
       //      divisible by the buy multiple?
       //      no need to log this if previous test above failed - 'a'
       if pr_convpak > 0 AND pr_buymult > 0
                         AND lg_logtype <> 'a';

          convpkw1 = pr_convpak/pr_buymult;
          convpkw2 = %rem(pr_convpak:pr_buymult);
          if convpkw2 <> 0;
             lg_logtype = 'b';
             lg_prvsupl = *blanks;
             lg_prvsub  = *blanks;
             lg_prvsplu = *blanks;
             lg_prvsplb = *blanks;
             lg_prvname = *blanks;
             sp_name    = saved_name;
             sp_buyr    = saved_buyr;
       //    ip_desc1   = *blanks;
       //    ip_desc1   = 'Conv Pack not a true multiple';
             exsr insert_logprod;
          endif;
       endif;

       //  weight logic
       //      don't allow weight divisor to be less than 1
       if ip_weightd  < 1;
          lg_logtype = 'S';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Weight divisor less than 1';
          exsr insert_logprod;
          ip_weightd = 1;
       endif;

       //      is weight being passed less than 0?
       if ip_weight  < 0;
          lg_logtype = 'R';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative weight being passed';
          exsr insert_logprod;
          ip_weight  = 0;
       endif;

       pr_weight  = ip_weight;
       pr_weightd = ip_weightd;

       if pr_weighto <> *zeros;
          pr_weight  = pr_weighto;
       endif;

       if pr_weighdo <> *zeros;
          pr_weightd = pr_weighdo;
       endif;

       //  volume logic
       //      don't allow volume divisor to be less than 1
       if ip_volumed  < 1;
          lg_logtype = 'U';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Volume divisor less than 1';
          exsr insert_logprod;
          ip_volumed = 1;
       endif;

       //    is volume being passed less than 0?
       if ip_volume  < 0;
          lg_logtype = 'T';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative volume being passed';
          exsr insert_logprod;
          ip_volume  = 0;
       endif;

       pr_volume  = ip_volume;
       pr_volumed = ip_volumed;

       if pr_volumeo <> *zeros;
          pr_volume  = pr_volumeo;
       endif;

       if pr_volumdo <> *zeros;
          pr_volumed = pr_volumdo;
       endif;

       //  accumulated demand information

       //      is data valid (negative sales being passed?)
       if ip_dlysale < 0;
          lg_logtype = 'D';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative sales of --->';
       // ip_dlysale = ip_dlysale * -1;
       // move      ip_dlysale    ip_desc1
          exsr insert_logprod;
          clear ip_dlysale;
       endif;

       //      is data valid (negative outs being passed?)
       if ip_dlyouts < 0;
          lg_logtype = 'E';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Negative outs of --->';
       // ip_dlyouts = ip_dlyouts * -1;
       // move      ip_dlyouts    ip_desc1
          exsr insert_logprod;
          clear ip_dlyouts;
       endif;

       //      if daily demand not sent in interface, then period-to-date
       //      demand is being passed, so clear accumulated fields
       if lc_daily = 0;
          clear pr_accsale;
          clear pr_accouts;
       endif;

       //      add daily sales
       pr_accsale += ip_dlysale;
       //      add daily lost sales
       pr_accouts += ip_dlyouts;
       //      demand for period
       pr_accdem  = pr_accsale + pr_accouts;

       //  do not include products excluded from 'count days' logic
       if pr_daysunt <> 999;

       //  do not include products not yet received
          if pr_lstrcvd <> not_recvd;

       //  determine if lost sales are used with 'count days out' logic
       //    and if any lost sales exist.
             if lost_sales = 1;
                if (pr_sysstat <> 'N') and
                   (pr_forcast > *zeros) and
                   (ip_dlyouts > *zeros);

                   pr_daysout += 1;
                endif;

             else;

       //  determine if back orders are used with 'count days out' logic
       //    and if any back orders exist.
                continue = 1;
                if back_order = 1 and pr_qtyback = 0;
                   continue = 0;
                endif;

       //  days out logic
                if (ip_qtyohnd <= pr_daysunt) and
                   (pr_sysstat <> 'N') and
                   (pr_forcast > *zeros) and
                   continue = 1;

                   pr_daysout += 1;
                endif;
       //     end of 'else' for lost_sales = 1
             endif;

       //     end for pr_lstrcvd <> not_recvd
          endif;

       //     end for pr_daysunt <> 999
       endif;

       //  clear delete count
       pr_deltcnt = 0;

       //  temporary split code
       pr_splttmp = 1;

       //  contract existance
       pr_contflg = ip_contflg;

       //  car count flag (must be 0=no or 1=yes)
       if car_update = 1;
          pr_carcoun = ip_carcoun;
          if pr_carcoun <> 0 and
             pr_carcoun <> 1;
             pr_carcoun =  0;
          endif;
       endif;

       //  transfer on order
       pr_trnoord = ip_trnoord;

       // Multi-location customer wants to send Transfer information
       if UpdQtyPend = 1;
          pr_qtypend = ip_trnoord;
       endif;

       //  alternate source on order
       pr_altoord = ip_altoord;

       //  update alternate source process flag
       //     has updating of field been blocked? 0=No
       if block_alts = 0;
       //     if value passed not a 0 or 1, then set to 0
          if ip_procalt <> 0 AND ip_procalt <> 1;
             lg_logtype = 'X';
             lg_prvsupl = *blanks;
             lg_prvsub  = *blanks;
             lg_prvsplu = *blanks;
             lg_prvsplb = *blanks;
             lg_prvname = *blanks;
             sp_name    = saved_name;
             sp_buyr    = saved_buyr;
       //    ip_desc1   = *blanks;
       //    ip_desc1   = 'Invalid flag alt srce process';
             ip_procalt = 0;
             exsr insert_logprod;
          endif;
          pr_procalt = ip_procalt;
       endif;

       //  clear exclude buying until after expiration?
       if clr_record = 1 AND
          pr_excuntl < lc_sysdate;

          clear pr_excuntl;
       endif;

       //  clear 3 promo fields after expiration?
       if clr_promo  = 1 AND
          pr_promend <> not_0001 and
          pr_promend < lc_sysdate;

          clear pr_promqty;
          clear pr_prombeg;
          clear pr_promend;
       endif;

       // --------------------------------------------------------- deal logic

       //  if a deal exists for the product being updated, and
       //     we have passed deal end date, then remove deal ID from product
       //     record, and determine if another deal exists that starts after
       //     the deal ID being removed.

       if pr_deal <> *blanks AND
          lc_sysdate > pr_dealend;

       //     go check for another deal
          pr_deal = *blanks;
          pr_dealbeg = pr_dealend;
          callp K3S_5120(ip_comp:
                         ip_locn:
                         ip_supl:
                         ip_suplsub:
                         ip_prod:
                         pr_deal:
                         pr_dealbeg:
                         pr_dealend:
                         pr_dealalw:
                         pr_dealuse);

       //     if another upcoming deal does not exist for this product,
       //        then clear fields in product record.
          if pr_deal = *blanks;
             clear pr_dealbeg;
             clear pr_dealend;
             clear pr_dealalw;
             clear pr_dealuse;
          endif;

       endif;

       // ----------------------------------------------------- end of period?

       //  if period has changed, then save history and update forecast
       //     also, total up the number of products processed by periodicity
       select;

       //       weekly forecasting
          when pr_forcint =  52;
               forcper = lc_perd52;
               total_52 += 1;

       //       monthly forecasting
          when pr_forcint =  12;
               forcper = lc_perd12;
               total_12 += 1;

       //       thirteen four-weekly forecasting
          when pr_forcint =  13;
               forcper = lc_perd13;
               total_13 += 1;

       endsl;

       //       period in product file different from location file, end perd
       if pr_forcper <> forcper;
          exsr $_end_perd;

       else;
       //   'Not end of period' processing

       //        if product is coded as Frozen,
       //        then check Today's date is greater than Frozen date
       //        clear frozen information
          if pr_usrstat = 'F';
             if lc_sysdate >  pr_forfrez;
                clear pr_usrstat;
                clear pr_forfrez;
             endif;
          endif;

       endif;

       //   If product is coded as Probation,
       //      then check Today's date is greater than Probation date
       //           clear probation information and log
       if pr_usrstat = 'P';
          if lc_sysdate >  pr_probdat;
             clear pr_usrstat;
             clear pr_probdat;
       //    lg_logtype = 'P';
             lg_logtype = '9';
             lg_prvsupl = *blanks;
             lg_prvsub  = *blanks;
             lg_prvsplu = *blanks;
             lg_prvsplb = *blanks;
             lg_prvname = *blanks;
             sp_name    = saved_name;
             sp_buyr    = saved_buyr;
             exsr insert_logprod;
          endif;
       endif;

       //       annual sales dollars and units
       pr_ansaleu = pr_forcast * pr_forcint;
       eval(h)   pr_ansale$ = pr_ansaleu * pr_sales;

       //    is customer updating PR_LSTRCVD 'last received date' daily?
       if lstrcvd = 1;
          exec sql
            select pl_ordrecv
              into :plordrecv
              from k_prodltm
              where pl_comp = :pr_comp and
                    pl_locn = :pr_locn and
                    pl_supl = :pr_supl and
                    pl_suplsub = :pr_suplsub and
                    pl_prod = :pr_prod
              fetch first row only;
          if SQLState = SQLStateOk AND
             plordrecv < days_10out AND
             plordrecv > pr_lstrcvd;

             pr_lstrcvd = plordrecv;
          endif;
       endif;

       //    is customer updating PR_RFBIRTH 'Reference Birth' daily?
       if ref_birth = 1;
          if pr_rfbirth = not_recvd  AND
             pr_qtyohnd > 0;

             pr_rfbirth = lc_sysdate;
          endif;
       endif;

       endsr;

       // /////////////////////////////////////////////////// Switch suppliers

       //  switch product to another supplier

       begsr $_switch;

       // pass flag back to night job to control process of changing
       // supplier ID's for alt source orders that have been saved during
       // the week.
       supl_chg = '1';

       // force getting supplier again on next product transaction input recd
       suplsaved  = *blanks;

       // prime key list for previous supplier information
       sx_comp = px_comp;
       sx_locn = px_locn;
       sx_supl = px_supl;
       sx_suplsub = px_suplsub;

       exec sql
         select *
           into :suplier_rec
           from k_suplier
           where sp_comp = :sx_comp and
                 sp_locn = :sx_locn and
                 sp_supl = :sx_supl and
                 sp_suplsub = :sx_suplsub
           fetch first row only;

       // put entry into log file for supplier change to product
       lg_logtype = '2';
       lg_prvsupl = sp_supl;
       lg_prvsub  = sp_suplsub;
       lg_prvsplu = sp_supl;
       lg_prvsplb = sp_suplsub;
       lg_prvname = sp_name;
       sp_name    = saved_name;
       sp_buyr    = saved_buyr;
       exsr insert_logprod;

       //  change product's history file records to reflect new supplier

       if pr_forcint = 12 OR pr_forcint = 13;

       //     monthly and 13-four weekly products
          callp K3S_3510(ip_comp:
                         ip_locn:
                         px_supl:
                         px_suplsub:
                         ip_prod:
                         lc_sysdate:
                         ip_supl:
                         ip_suplsub:
                         ip_suplusr:
                         ip_suplusb);

       else;

       //     weekly products
          callp K3S_3511(ip_comp:
                         ip_locn:
                         px_supl:
                         px_suplsub:
                         ip_prod:
                         lc_sysdate:
                         ip_supl:
                         ip_suplsub:
                         ip_suplusr:
                         ip_suplusb);

       endif;

       //  change supplier information
       pr_supl    = ip_supl;
       pr_suplsub = ip_suplsub;
       pr_suplusr = ip_suplusr;
       pr_suplusb = ip_suplusb;
       pr_suplorg = ip_supl;
       pr_suplors = ip_suplsub;
       pr_buyr    = saved_buyr;

       //  use new supplier's lead time information
       pr_leadtm  = saved_lt;
       pr_leadtmv = saved_ltv;
       clear pr_leadtms;

       //  clear deal ID fields
       clear pr_deal;
       clear pr_dealbeg;
       clear pr_dealend;
       clear pr_dealalw;
       clear pr_dealuse;

       //write new product information
       exsr $_update;
       exsr insert_product;


       //   go check for deals
       if copy_deals = 1;
       //   but make sure new product NOT under Hub Transfer Supplier
          trans_prod = 0;
          exsr $_transfer;
          if trans_prod = 0;
       //   if Bindley, make sure its not location 06
             if no_deal_06 = 0 OR
                (no_deal_06 = 1 AND pr_locn <> '06');

                night = 'Y';
                callp K3S_5056(pr_comp:
                               pr_locn:
                               pr_prod:
                               pr_supl:
                               pr_suplsub:
                               night);
             endif;
          endif;
       endif;

       //  prime key list for removal of product record that was copied
       exec sql
         delete k_product
           where pr_comp = :px_comp and
                 pr_locn = :px_locn and
                 pr_supl = :px_supl and
                 pr_suplsub = :px_suplsub and
                 pr_prod    = :px_prod;

       endsr;

       ////////////////////////////////////////////////////////// New product

       //  add new product to database

       begsr $_newprod;


       // key information
       pr_comp    = ip_comp;
       pr_locn    = ip_locn;
       pr_supl    = ip_supl;
       pr_suplsub = ip_suplsub;
       pr_suplusr = ip_suplusr;
       pr_suplusb = ip_suplusb;
       pr_suplorg = ip_supl;
       //pr_suplors = *blanks;
       pr_suplors = ip_suplsub;
       pr_prod    = ip_prod;

       //  default buyer from supplier
       pr_buyr    = sp_buyr;

       //  birth date from location file
       pr_birth   = lc_sysdate;
       //    or from IP_BIRTH based upon APP 'K3S_9010  PR_BIRTH' setting
       if NewBirth = 1;
          pr_birth   = ip_birth;
       endif;

       //  lead time from supplier file
       pr_leadtm  = sp_leadtmo;
       pr_leadtmv = sp_leadtmv;

       //  system status as NEW
       pr_sysstat = 'N';

       //  automatic change of system status from 'N' to 'S'?
       if new_slow = 1;
          pr_sysstat = 'S';
       //         default average for new products
          if new_usedft = 1 and
             new_avgdft > 0;

             pr_forcast = new_avgdft;
          endif;
       endif;

       //  long term trend rate to 1.00

       pr_longtrm = 1.00;

       //  line cost from location file
       pr_linecst = sp_linecst;

       //  convenience pack % from location file
       pr_convpkp = sp_convpkp;

       //  ranking information
       pr_rnkbuy$ = 0;
       pr_rnkbuyu = 0;
       pr_rnkloc$ = 0;
       pr_rnklocu = 0;
       pr_rnkcom$ = 0;
       pr_rnkcomu = 0;
       pr_rnksup$ = 0;
       pr_rnksupu = 0;
       pr_rnkdate = lc_sysdate;

       //  purchase order quantity divisor to 1
       pr_poqtydv = 1;

       //  dev% value from location file
       pr_fordevp = lc_fordevp;

       //  previous dev% value from location file
       //    this would be helpful if previous averages need to be
       //    restored, and there are new products that had no prv value
       pr_prvdevp = lc_fordevp;

       //  investment method from company file
       pr_invmeth = cminvmeth;

       //  forecast method from company file
       pr_formeth = cmformeth;

       //  forecast interval

       //      is forecast interval code being passed a valid one?
       //      if not, default to 2 for monthly
       //         should be:  1 = weekly
       //                     2 = monthly
       if ip_forctyp < 1 OR ip_forctyp > 3;
          lg_logtype = 'W';
          lg_prvsupl = *blanks;
          lg_prvsub  = *blanks;
          lg_prvsplu = *blanks;
          lg_prvsplb = *blanks;
          lg_prvname = *blanks;
          sp_name    = saved_name;
          sp_buyr    = saved_buyr;
       // ip_desc1   = *blanks;
       // ip_desc1   = 'Invalid forecast cycle code';
          ip_forctyp = 2;
          exsr insert_logprod;
       endif;

       select;

       //       weekly forecasting
          when ip_forctyp = 1;
               pr_forcint = 52;
               pr_forcper = lc_perd52;
               pr_forcyr  = lc_year52;

       //       monthly forecasting
          when ip_forctyp = 2;
               pr_forcint = 12;
               pr_forcper = lc_perd12;
               pr_forcyr  = lc_year12;

       //       thirteen four-weekly forecasting
          when ip_forctyp = 3;
               pr_forcint = 13;
               pr_forcper = lc_perd13;
               pr_forcyr  = lc_year13;

       endsl;

       //  permanent split code to 1
       pr_spltprm = 1;

       //  initialize last cost to 0, so update routine will save last date
       pr_costlst = 0;

       //  use supplier service level target as default for new product
       pr_service = sp_service;

       //  check to see if history already exists for this product
       callp K3S_3037(pr_comp:
                      pr_locn:
                      pr_suplusr:
                      pr_suplusb:
                      pr_prod:
                      pr_forcint:
                      pr_fstslyr:
                      pr_fstslpr);

       //  create a new product history record, if no previous history exists
       //    or if first sales year is not this current year, then go see if
       //    a new history record for this year is needed
       if pr_fstslyr = 0 or
          pr_fstslyr <> pr_forcyr;

          exsr $_add_hist;
       endif;

       //       put entry into log file for addition of new product
       lg_logtype = '1';
       lg_prvsupl = *blanks;
       lg_prvsub  = *blanks;
       lg_prvname = *blanks;
       exsr insert_logprod;

       //       maximum forward buy days
       pr_maxdays = sp_maxdays;

       //       always assume new products can be purchased from diverter,
       //       regardless of what the APP record 'K3S_9010  BLOCK_ALTS' says,
       //       since buyer can then change setting in K3S_3000 '1 of 5'
       pr_procalt = 1;

       //    Assignment of Corporate Profiles
       if corp_prof = 1;
          pr_seasonl = CorpProfID;
          pr_seassrc = 'S';
          pr_seasact = lc_sysdate;
       endif;

       pr_costreg = 0;
       pr_ansaleu = 0;
       pr_buymulo = 0;
       pr_minqty  = 0;
       pr_minqtyo = 0;
       pr_purincr = 0;
       pr_purinco = 0;
       pr_disotho = 0;
       pr_disothr = 0;
       pr_disun7o = 0;
       pr_disunt7 = 0;
       pr_disun8o = 0;
       pr_disunt8 = 0;
       pr_disun9o = 0;
       pr_disunt9 = 0;
       pr_convpko = 0;
       pr_convpak = 0;
       pr_weight  = 0;
       pr_weightd = 0;
       pr_weighto = 0;
       pr_weighdo = 0;
       pr_volume  = 0;
       pr_volumed = 0;
       pr_volumeo = 0;
       pr_volumdo = 0;
       pr_accsale = 0;
       pr_accouts = 0;
       pr_accdem  = 0;
       pr_daysunt = 0;
       pr_carcoun = 0;
       pr_trnoord = 0;
       pr_altoord = 0;
       pr_dealalw = 0;
       pr_dealuse = 0;

       pr_altsrce = 0;
       pr_leadtmp = 0;
       pr_leadtms = 0;
       pr_leadtmt = 0;
       pr_brkflag = 0;
       pr_brkcost = 0;
       pr_saleslw = 0;
       pr_qtyhold = 0;
       pr_forserr = 0;
       pr_daysout = 0;
       pr_daysprv = 0;
       pr_intrval = 0;
       pr_otimfac = 0;
       pr_maminid = 0;
       pr_mamaxid = 0;
       pr_maminiu = 0;
       pr_mamaxiu = 0;
       pr_add_day = 0;
       pr_devtimf = 0;
       pr_sstimef = 0;
       pr_safesum = 0;
       pr_svceprv = 0;
       pr_servsim = 0;
       pr_histper = 0;
       pr_promper = 0;
       pr_prvfore = 0;
       pr_prvserr = 0;
       pr_prvts   = 0;
       pr_prvlost = 0;
       pr_prvsale = 0;
       pr_prvexpt = 0;
       pr_prvdemd = 0;
       pr_tranptd = 0;
       pr_tranlst = 0;
       pr_repcary = 0;
       pr_exclead = 0;
       pr_nonstck = 0;
       pr_acctyp1 = 0;
       pr_acctyp2 = 0;
       pr_acctyp3 = 0;
       pr_acctyp4 = 0;
       pr_acctyp5 = 0;
       pr_acctyp6 = 0;
       pr_acctyp7 = 0;
       pr_acctyp8 = 0;
       pr_acctyp9 = 0;
       pr_usern1  = 0;
       pr_usern2  = 0;
       pr_usern3  = 0;
       pr_tempory = 0;
       pr_promqty = 0;

       pr_lstrcvd = init_date;
       pr_excuntl = init_date;
       pr_excaftr = init_date;
       pr_costldt = init_date;
       pr_formanl = init_date;
       pr_custusg = init_date;
       pr_forfrez = init_date;
       pr_lstordr = init_date;
       pr_fbuydat = init_date;
       pr_probdat = init_date;
       pr_dealbeg = init_date;
       pr_dealend = init_date;
       pr_rfbirth = init_date;
       pr_prombeg = init_date;
       pr_promend = init_date;
       pr_seasact = init_date;

       endsr;

       ////////////////////////////////////////////////////////// End period

       //  end of period

       begsr $_end_perd;

       // save off 'before picture' for PE snapshot capture
       if pe_snpshot = 1;
          pf_comp    = pr_comp;
          pf_locn    = pr_locn;
          pf_supl    = pr_supl;
          pf_suplsub = pr_suplsub;
          pf_prod    = pr_prod;

          if first_shot = 0;
             first_shot = 1;

             callp K3S_Retrieve_Timestamp(time_stamp);

             #birth =  %date(%subst(%char(time_stamp):1:10):*ISO);
             pf_birth = #birth;

             #birthtm = %time(%subst(%char(time_stamp):12:8):*ISO);
             pf_birthtm = #birthtm;

             pf_chgtype = 'G';
             pf_chgdesc = 'Period End snapshots';
             pf_user    = 'K3S Night';
             pf_workstn = 'Batch job';
             pf_program = 'K3S_9010  ';
          endif;
          pf_seasbef = pr_seasonl;
          pf_seasaft = pr_seasonl;
          pf_avgbef  = pr_forcast;
          pf_devpbef = pr_fordevp;
          pf_statbef = pr_usrstat;
          pf_stataft = pr_usrstat;
       endif;

       //------------------------------------ retrieve seasonal profile factor
       //  retrieve seasoal profile factor for one period only
       //    (default value to 1.00 if not seasonal)

       if pr_seasonl <> *blanks;

       // call module to retrieve the seasonal profile factor for one period
          callp K3S_M051(pr_comp:
                         pr_seasonl:
                         pr_forcper:
                         seas_factr);

       //    default value to 1.00 if not seasonal
       else;
          seas_factr = 1.00;

       endif;

       //--------------------------------------- days out logic for lost sales
       //  calculate lost sales based upon days out of stock
       //    (product uses either accumulated PTD outs, or days out, but
       //     not both!)

       //  is days out logic being used?
       if lc_daysusd = 1;

          pr_accouts = 0;

          select;

       //       monthly forecasting
             when pr_forcint = 12;
                  days_perd  = lc_daysp12;

       //       thirteen four-weekly forecasting
             when pr_forcint = 13;
                  days_perd  = lc_daysp13;

       //       weekly forecasting
             when pr_forcint = 52;
                  days_perd  = lc_daysp52;

          endsl;

       //   days out can't be greater than days in period
          if pr_daysout > days_perd;
             pr_daysout = days_perd;
          endif;

       //   calculate lost percent
          if days_perd > 0;
             eval(h)   lost_perct = pr_daysout / days_perd;
          endif;

       //   calculate lost sales
          if pr_daysunt <> 999;
             eval(h)   pr_accouts = pr_forcast * lost_perct *
                                    seas_factr;
          endif;

       //   re-calculate accumulated demand
          pr_accdem  = pr_accouts + pr_accsale;

       endif;

       //------------------------------------------------- save demand history
       //  save accumulated demand into product history file, and determine
       //       if this is the first sale ever for this product
       if pr_accdem > 0;
          clear histype;

       // call program to save accumulated demand into product history file
          if pr_forcint = 12 OR pr_forcint = 13;

       //      monthly or 13 four-weekly products
             callp K3S_3500(pr_comp:
                            pr_locn:
                            pr_supl:
                            pr_suplsub:
                            pr_suplusr:
                            pr_suplusb:
                            pr_prod:
                            pr_forcyr:
                            pr_forcint:
                            histype:
                            pr_forcper:
                            pr_accdem:
                            lc_sysdate);

          else;

       //     weekly products
             callp K3S_3501(pr_comp:
                            pr_locn:
                            pr_supl:
                            pr_suplsub:
                            pr_suplusr:
                            pr_suplusb:
                            pr_prod:
                            pr_forcyr:
                            pr_forcint:
                            histype:
                            pr_forcper:
                            pr_accdem:
                            lc_sysdate);

          endif;

          if pr_fstslyr = 0;
             pr_fstslyr = pr_forcyr;
             pr_fstslpr = pr_forcper;
          endif;

       endif;

       //---------------------------- save previous period forecast components
       //  save previous period forecast components

       //  ensure no negative values get saved for Previous fields
       if pr_accsale < 0 OR
          pr_accouts < 0 OR
          pr_accdem  < 0;

          pr_accsale = 0;
          pr_accouts = 0;
          pr_accdem  = 0;
       endif;

       pr_prvdemd = pr_accdem;
       pr_prvfore = pr_forcast;
       pr_prvdevp = pr_fordevp;
       pr_prvserr = pr_forserr;
       // pr_prvts   = pr_prvserr /
       //              (pr_prvdevp * .01);
       pr_prvlost = pr_accouts * pr_sales;
       pr_prvsale = pr_accsale * pr_sales;
       pr_prvexpt = pr_prvfore * pr_sales *
                    seas_factr;

       //----------------------------------------------------- update forecast
       //  update forcast components

       clear error_type;

       //  check if forecast has been frozen until today?
       if lc_sysdate >= pr_forfrez;

       // any products that had been frozen are cleared
          if pr_usrstat = 'F';
             clear pr_usrstat;
             clear pr_forfrez;
          endif;

       // convert values to be passed from 3.1 to 3.3 fields
          fordevp = pr_fordevp * .01;
          nodevp  = lc_nodevp  * .01;

          accdem  = pr_accdem;
          forcast = pr_forcast;
          forserr = pr_forserr;

       //   don't allow demand to be negative when passed to K3S_M080
          if accdem  < 0;
             accdem  = 0;
          endif;

       //  By-pass forecast routine when Minimum Seasonal Factor logic
       //     being used, and this period is low
          if (min_used = 1 AND
             seas_factr >= min_season) OR
             min_used = 0;

       // call module to update forcast components
             callp K3S_M080(seas_factr:
                            accdem:
                            forcast:
                            fordevp:
                            forserr:
                            pr_longtrm:
                            lc_demhi:
                            demlo:
                            lc_smfactr:
                            lc_tslimit:
                            nodevp:
                            error_type:
                            lc_avgzero);
          endif;

       //         calculate absolute value of smoothed error to be used below
       //         for tests
          if forserr < 0;
             forserr_AV = forserr * -1;
          else;
             forserr_AV = forserr;
          endif;

       // If forecast is negative, or deviation is negative, or
       //    absolute value of smoothed error is greater than deviation %,
       //    then don't update product values, but log problem.
       //      change on 1/25/99 now self cleanses T/S problem

          if forcast < 0 or
             fordevp < 0 or
             forserr_AV >= fordevp;

       // If forecast going negative, log problem
             if forcast < 0;
                lg_logtype = 'A';
                lg_prvsupl = *blanks;
                lg_prvsub  = *blanks;
                lg_prvsplu = *blanks;
                lg_prvsplb = *blanks;
                lg_prvname = *blanks;
                exsr insert_logprod;
             endif;

       // If deviation going negative, log problem
             if fordevp < 0;
                lg_logtype = 'B';
                lg_prvsupl = *blanks;
                lg_prvsub  = *blanks;
                lg_prvsplu = *blanks;
                lg_prvsplb = *blanks;
                lg_prvname = *blanks;
                exsr insert_logprod;
             endif;

       //  If absolute value of smoothed error is greater than devaition %
             if forserr_AV >= fordevp;
                lg_logtype = 'C';
                lg_prvsupl = *blanks;
                lg_prvsub  = *blanks;
                lg_prvsplu = *blanks;
                lg_prvsplb = *blanks;
                lg_prvname = *blanks;
       //       exsr $_log_prod;
                pr_forserr = 0;
             endif;

          else;

       //  update product values
             pr_forcast = forcast;
             pr_fordevp = fordevp * 100;
             pr_forserr = forserr;
          endif;

       //  check if forecast has been frozen
       endif;

       //------------------------------- save previous period service acheived
       //  save previous period service level acheived

       if pr_accdem > 0 AND pr_accdem >= pr_accsale;
          pr_svceprv = (pr_accsale/pr_accdem) * 100;
       else;
          clear pr_svceprv;
       endif;

       //  If there was no demand in previous period then make previous service
       //  level 100%
       if pr_accdem = 0;
          pr_svceprv = 100;
       endif;

       //---------------------------------------------- update period and year
       //  period has changed, so save correct current period and year
       //         also, total up the number of products that have rolled
       //         by periodicity

       select;

       //      weekly forecasting
          when pr_forcint = 52;

       //          if this is year end, then create a new history record
               if pr_forcyr <> lc_year52;
                  pr_forcyr  = lc_year52;
                  exsr $_add_hist;
                  ye_for_52  = '1';
               endif;

               pr_forcyr  = lc_year52;
               pr_forcper = lc_perd52;
               rolled_52 += 1;

       //       monthly forecasting
          when pr_forcint = 12;

       //          if this is year end, then create a new history record
               if pr_forcyr <> lc_year12;
                  pr_forcyr  = lc_year12;
                  exsr $_add_hist;
                  ye_for_12  = '1';
               endif;

               pr_forcyr  = lc_year12;
               pr_forcper = lc_perd12;
               rolled_12 += 1;

       //       thirteen four-weekly forecasting
          when pr_forcint = 13;

       //          if this is year end, then create a new history record
               if pr_forcyr <> lc_year13;
                  pr_forcyr  = lc_year13;
                  exsr $_add_hist;
                  ye_for_13  = '1';
               endif;

               pr_forcyr  = lc_year13;
               pr_forcper = lc_perd13;
               rolled_13 += 1;

       endsl;

       //------------------------------------------------ update system status
      /copy k3s_c160

       //-------------------------------------------------- end of period code
       //  end of period code

       //     clear end of period code
       pr_endper = *blanks;

       //     only regular, lumpy, or slow products into 1st five categories
       //       1=DF high, 2=DF low, 3=TS high, 4=TS low, 5=Service
       //       then discontinued or new products are tested
       //          6=New, 7=Discontinued

       if (pr_sysstat = 'R') or
          (pr_sysstat = 'L') or
          (pr_sysstat = 'S');

          select;

       //     demand filter high
             when error_type = '1';
                  pr_endper  = '1';

       //     demand filter low
             when error_type = '2';
                  pr_endper  = '2';

       //     tracking signal high trending up
             when error_type = '3';
                  pr_endper  = '3';

       //     tracking signal low  trending down
             when error_type = '4';
                  pr_endper  = '4';

       //     service level check
             other;

                  if pr_accdem > 0 AND
                     pr_svceprv < pr_service;

                     pr_endper  = '5';
                  endif;

          endsl;

       //     if Exclude Buying Until in effect and more than 90 days out,
       //        and this product had fallen into PE1 through PE5,
       //        then buyer does not need to see this as PE check for now
          if pr_excuntl > Ninety_Out and
             pr_endper <> *blanks;

             clear pr_endper;
          endif;

       else;
       //   test for new and discontinued products
       //   new products
          if pr_sysstat = 'N';
             pr_endper  = '7';
          endif;

       //     discontinued products
          if pr_sysstat = 'D';
             pr_endper  = '9';
          endif;

       endif;

       //     un-conditionally make watch products category 6
       if pr_usrstat = 'W';
          pr_endper  = '6';
       endif;

       //     un-conditionally make manual products category 8
       if pr_usrstat = 'M';
          pr_endper  = '8';
       endif;

       //     save end of period code
       pr_endpers = pr_endper;

       //     Filter PE1 checks?
       if PE1_FLTR_1 =  1  and
          pr_endper  = '1' and
          pr_maminiu >  0  and
          pr_prvdemd <= pr_maminiu;

          pr_endper = *blanks;
       endif;

       //     save transfers period-to-date into last period transfers
       pr_tranlst = pr_tranptd;
       clear pr_tranptd;

       //     save previous period days out
       pr_daysprv = pr_daysout;

       //-------------------------------------------------- clear accumulators
       //  clear accumulators
       clear pr_accsale;
       clear pr_accouts;
       clear pr_accdem;
       clear pr_daysout;

       // save off 'after picture' for PE snapshot capture
       if pe_snpshot = 1;
          pf_sysstat = pr_sysstat;
          pf_devpaft = pr_fordevp;
          pf_avgaft  = pr_forcast;
          pf_avgdiff = pf_avgaft - pf_avgbef;
          exec sql
             select count(*)
               into :prodfor_cnt
               from k_prodfor
               where pf_comp = :pr_comp AND
                     pf_locn = :pr_locn AND
                     pf_supl = :pr_supl AND
                     pf_suplsub = :pr_suplsub AND
                     pf_prod = :pr_prod AND
                     pf_birth = :#birth AND
                     pf_birthtm = :#birthtm;
          if prodfor_cnt = 0;
             pf_hstmult = 0;
             exsr insert_prodfor;
          endif;
       endif;

       endsr;

       ///////////////////////////////////////////// Log Product Transactions

       //  Log of Product Transactions

       begsr insert_logprod;

       select;

          when lg_logtype = '1';
               lda_rctyp1 += 1;

          when lg_logtype = '2';
               lda_rctyp2 += 1;

          when lg_logtype = '5';
               lda_rctyp5 += 1;

          when lg_logtype = '6';
               lda_rctyp6 += 1;

          when lg_logtype = '7';
               lda_rctyp7 += 1;

          when lg_logtype = '8';
               lda_rctyp8 += 1;

          when lg_logtype = '9';
               lda_rctyp9 += 1;

          other;
               lda_rctypx += 1;

       endsl;

       lg_comp    = ip_comp;
       lg_locn    = ip_locn;
       lg_buyr    = sp_buyr;
       lg_supl    = ip_supl;
       lg_suplsub = ip_suplsub;
       lg_suplusr = ip_suplusr;
       lg_suplusb = ip_suplusb;
       lg_suplnam = sp_name;
       lg_prod    = ip_prod;
       lg_desc1   = ip_desc1;
       lg_deltcnt = pr_deltcnt;

       // call module to retrieve timestamp
       callp K3S_Retrieve_Timestamp(time_stamp);
       lg_timestp = time_stamp;

       // if customer has a method to get supplier name and buyer id for
       //    log type '5' records, then exit to customer's system to get
       //    data.
       if get_log_5 = '1' AND lg_logtype = '5';
          callp K3S_9010X1(ip_comp:
                           ip_locn:
                           ip_suplusr:
                           ip_suplusb:
                           ip_prod:
                           lg_buyr:
                           lg_suplnam);
       endif;

       exec sql
            insert into k_logprod
              values (:logprod_rec);

       endsr;

       ///////////////////////////////////////////////// Add Product History

       //  Add product history record

       begsr $_add_hist;

       clear histype;

       // call program to create a new product history record
       if pr_forcint = 12 OR pr_forcint = 13;

       //     monthly and 13-four weekly products
          callp K3S_3505(pr_comp:
                         pr_locn:
                         pr_suplusr:
                         pr_suplusb:
                         pr_prod:
                         pr_forcyr:
                         pr_forcint:
                         histype:
                         lc_sysdate:
                         pr_supl:
                         pr_suplsub);

       else;

       //     weekly products
          callp K3S_3506(pr_comp:
                         pr_locn:
                         pr_suplusr:
                         pr_suplusb:
                         pr_prod:
                         pr_forcyr:
                         pr_forcint:
                         histype:
                         lc_sysdate:
                         pr_supl:
                         pr_suplsub);

       endif;

       endsr;

       ////////////////////////////////////////////////////// Trans supplier?

       begsr $_transfer;

       // test for supplier record

       exec sql
         select count(*)
           into :transpl_cnt
             from k_transpl
             where ts_comp = :pr_comp and
                   ts_locn = :pr_locn and
                   ts_supl = :pr_suplusr and
                   ts_suplsub = :pr_suplusb;

       if transpl_cnt > 0;  //row found
          trans_prod = 1;
       endif;
       if transpl_cnt = 0;  //row not found

       // test for product record
          exec sql
            select count(*)
              into :tranprd_cnt
                from k_tranprd
                where tp_comp = :pr_comp and
                      tp_locn = :pr_locn and
                      tp_prod = :pr_prod;
          if tranprd_cnt > 0;
             trans_prod = 1;
          endif;

       endif;

       endsr;
       /////////////////////////////////////////////////////// write prodlog

       begsr $_prodlog;


       if pr_altsrce = 0 and
          pr_deltcnt = 0 and
          pr_tempory = 0;

          callp K3S_Retrieve_Timestamp(time_stam1);
          date = %date(%subst(%char(time_stam1):1:10):*ISO);
          time = %time(%subst(%char(time_stam1):12:8):*iso);

          p0_comp    = pr_comp;
          p0_locn    = pr_locn;
          p0_supl    = pr_supl;
          p0_suplsub = pr_suplsub;
          p0_prod    = pr_prod;
          p0_repcary = pr_repcary;
          p0_prombeg = pr_prombeg;
          p0_promend = pr_promend;
          p0_forfrez = pr_forfrez;
          p0_probdat = pr_probdat;
          p0_excuntl = pr_excuntl;
          p0_prodseq = pr_prodseq;
          p0_desc2   = pr_desc2;
          p0_ndc_upc = pr_ndc_upc;
          p0_group1o = pr_group1o;
          p0_group2o = pr_group2o;
          p0_group3o = pr_group3o;
          p0_group4o = pr_group4o;
          p0_group5o = pr_group5o;
          p0_mfg     = pr_mfg;
          p0_maminid = pr_maminid;
          p0_maminiu = pr_maminiu;
          p0_mamaxid = pr_mamaxid;
          p0_mamaxiu = pr_mamaxiu;
          p0_catalog = pr_catalog;
          p0_uom     = pr_uom;
          p0_packsiz = pr_packsiz;
          p0_tihi    = pr_tihi;
          p0_leadtm  = pr_leadtm;
          p0_leadtmv = pr_leadtmv;
          p0_whslocn = pr_whslocn;
          p0_linecst = pr_linecst;
          p0_ovrcreg = pr_ovrcreg;
          p0_ovrcdiv = pr_ovrcdiv;
          p0_promqty = pr_promqty;
          p0_minqtyo = pr_minqtyo;
          p0_buymulo = pr_buymulo;
          p0_convpko = pr_convpko;
          p0_convpkp = pr_convpkp;
          p0_daysunt = pr_daysunt;
          p0_maxdays = pr_maxdays;
          p0_add_day = pr_add_day;
          p0_weighto = pr_weighto;
          p0_volumeo = pr_volumeo;
          p0_weighdo = pr_weighdo;
          p0_volumdo = pr_volumdo;
          p0_purinco = pr_purinco;
          p0_disothr = pr_disothr;
          p0_service = pr_service;
          p0_spltprm = pr_spltprm;
          p0_poqtydv = pr_poqtydv;
          p0_poqtyum = pr_poqtyum;
          p0_procalt = pr_procalt;
          p0_exclead = pr_exclead;
          p0_formeth = pr_formeth;
          p0_invmeth = pr_invmeth;
          p0_rebate  = pr_rebate;
          p0_birth = date;
          p0_birthtm = time;
          p0_chgtype = 'A';
          evalr p0_user = 'K3S_NIGHT ';
          evalr p0_workstn = 'K3S_NIGHT ';
          evalr p0_program = 'K3S_9010  ';

          exec sql
            select count(*)
              into :prodlog_cnt
              from k_prodlog
              where p0_comp = :pr_comp and
                    p0_locn = :pr_locn and
                    p0_supl = :pr_supl and
                    p0_suplsub = :pr_suplsub and
                    p0_prod = :pr_prod and
                    p0_birth = :date and
                    p0_birthtm = :time;
          if prodlog_cnt = 0;           //no rows found
             exsr insert_prodlog;
          endif;

       endif;

       endsr;

       begsr dclipcursor;

       exec sql
        declare ipcursor Cursor
         for
         select ip_comp, ip_locn, ip_birth, ip_lastupd, ip_supl, ip_suplsub,
                ip_suplusr, ip_suplusb, ip_prod, ip_prodseq, ip_desc1,
                ip_desc2, ip_mfg, ip_ndc_upc, ip_uom, ip_packsiz, ip_tihi,
                ip_status, ip_minqty, ip_buymult, ip_group1, ip_group2,
                ip_group3, ip_group4, ip_group5, ip_whslocn, ip_costreg,
                ip_costdiv, ip_sales, ip_saleslw, ip_qtyohnd, ip_qtyoord,
                ip_qtyback, ip_weight, ip_weightd, ip_volume, ip_volumed,
                ip_dlysale, ip_dlyouts, ip_trnoord, ip_altoord, ip_forctyp,
                ip_convpak, ip_purincr, ip_contflg, ip_rebate, ip_procalt,
                ip_mfgout, ip_dlytyp1, ip_dlytyp2, ip_dlytyp3, ip_dlytyp4,
                ip_dlytyp5, ip_dlytyp6, ip_dlytyp7, ip_dlytyp8, ip_dlytyp9,
                ip_carcoun, ip_usera1, ip_usera2, ip_usera3, ip_usern1,
                ip_usern2, ip_usern3, ip_disothr, ip_disunt7, ip_disunt8,
                ip_disunt9
         from k_intprod
         where ip_comp = :comp
         order by ip_comp,
                  ip_locn,
                  ip_supl,
                  ip_suplsub,
                  ip_prod
         for read only;

       endsr;

       begsr opnipcursor;

       exec sql
        open ipcursor;
        if SQLState <> SQLStateOk;
           exsr clsipcursor;
           exec sql
            open ipcursor;
        endif;
       endsr;

       begsr clsipcursor;
       exec sql
        close ipcursor;
       endsr;

       begsr dcllccursor;

       exec sql
        declare lccursor Cursor
         for
         select *
         from k_locatns
         where lc_comp = :ip_comp and
               lc_locn >= '     '
         order by lc_comp,
                  lc_locn;

       endsr;

       begsr opnlccursor;
       exec sql
        open lccursor;
       endsr;

       begsr clslccursor;
       exec sql
        close lccursor;
       endsr;

       begsr insert_prodlog;

         exec sql
           insert into k_prodlog
             values (:prodlog_rec);

       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Product +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrchPrdA;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pr_comp = ? and +
                     pr_locn = ? and +
                     pr_supl = ? and +
                     pr_suplsub = ? and +
                     pr_prod = ? +
                     Order by pr_comp, +
                              pr_locn, +
                              pr_supl, +
                              pr_suplsub, +
                              pr_prod +
                     For update of pr_soqnite, +
                                   pr_qtybaln, +
                                   pr_overflg, +
                                   pr_overunt, +
                                   pr_overcst, +
                                   pr_overcur, +
                                   pr_overmax, +
                                   pr_qtypend, +
                                   pr_buyr, +
                                   pr_suplorg, +
                                   pr_suplors, +
                                   pr_suplusr, +
                                   pr_suplusb, +
                                   pr_lastupd, +
                                   pr_lstintr, +
                                   pr_regn, +
                                   pr_mfgout, +
                                   pr_prodseq, +
                                   pr_desc1, +
                                   pr_desc2, +
                                   pr_mfg, +
                                   pr_catalog, +
                                   pr_ndc_upc, +
                                   pr_uom, +
                                   pr_packsiz, +
                                   pr_tihi, +
                                   pr_whslocn, +
                                   pr_sysstat, +
                                   pr_usrstat, +
                                   pr_forfrez, +
                                   pr_group1, +
                                   pr_group2, +
                                   pr_group3, +
                                   pr_group4, +
                                   pr_group5, +
                                   pr_group1o, +
                                   pr_group2o, +
                                   pr_group3o, +
                                   pr_group4o, +
                                   pr_group5o, +
                                   pr_ovrcreg, +
                                   pr_ovrcdiv, +
                                   pr_costlst, +
                                   pr_costldt, +
                                   pr_costreg, +
                                   pr_costdiv, +
                                   pr_costeac, +
                                   pr_sales, +
                                   pr_excuntl, +
                                   pr_qtyohnd, +
                                   pr_qtyoord, +
                                   pr_qtyback, +
                                   pr_buymuli, +
                                   pr_buymult, +
                                   pr_minqty, +
                                   pr_purincr, +
                                   pr_disothr, +
                                   pr_disunt7, +
                                   pr_disunt8, +
                                   pr_disunt9, +
                                   pr_rebate, +
                                   pr_convpak, +
                                   pr_weight, +
                                   pr_weightd, +
                                   pr_volume, +
                                   pr_volumed, +
                                   pr_accsale, +
                                   pr_accouts, +
                                   pr_accdem, +
                                   pr_daysout, +
                                   pr_daysprv, +
                                   pr_deltcnt, +
                                   pr_splttmp, +
                                   pr_contflg, +
                                   pr_carcoun, +
                                   pr_trnoord, +
                                   pr_altoord, +
                                   pr_procalt, +
                                   pr_promqty, +
                                   pr_prombeg, +
                                   pr_promend, +
                                   pr_deal, +
                                   pr_dealbeg, +
                                   pr_dealend, +
                                   pr_dealalw, +
                                   pr_dealuse, +
                                   pr_probdat, +
                                   pr_ansaleu, +
                                   pr_ansale$, +
                                   pr_rfbirth, +
                                   pr_lstrcvd, +
                                   pr_leadtm, +
                                   pr_leadtmv, +
                                   pr_leadtms, +
                                   pr_forcper, +
                                   pr_endper, +
                                   pr_forcast, +
                                   pr_forserr, +
                                   pr_prvsale, +
                                   pr_prvlost, +
                                   pr_prvserr, +
                                   pr_prvfore, +
                                   pr_endpers, +
                                   pr_svceprv, +
                                   pr_prvexpt, +
                                   pr_prvdevp, +
                                   pr_prvdemd, +
                                   pr_fordevp';

       endsr;

       begsr update_producta;

       exec sql
          update k_product
            set pr_soqnite = :pr_soqnite,
                pr_qtybaln = :pr_qtybaln,
                pr_overflg = :pr_overflg,
                pr_overunt = :pr_overunt,
                pr_overcst = :pr_overcst,
                pr_overcur = :pr_overcur,
                pr_overmax = :pr_overmax,
                pr_qtypend = :pr_qtypend,
                pr_buyr    = :pr_buyr,
                pr_suplorg = :pr_suplorg,
                pr_suplors = :pr_suplors,
                pr_suplusr = :pr_suplusr,
                pr_suplusb = :pr_suplusb,
                pr_lastupd = :pr_lastupd,
                pr_lstintr = :pr_lstintr,
                pr_regn    = :pr_regn,
                pr_mfgout  = :pr_mfgout,
                pr_prodseq = :pr_prodseq,
                pr_desc1   = :pr_desc1,
                pr_desc2   = :pr_desc2,
                pr_mfg     = :pr_mfg,
                pr_catalog = :pr_catalog,
                pr_ndc_upc = :pr_ndc_upc,
                pr_uom     = :pr_uom,
                pr_packsiz = :pr_packsiz,
                pr_tihi    = :pr_tihi,
                pr_whslocn = :pr_whslocn,
                pr_sysstat = :pr_sysstat,
                pr_usrstat = :pr_usrstat,
                pr_forfrez = :pr_forfrez,
                pr_group1  = :pr_group1,
                pr_group2  = :pr_group2,
                pr_group3  = :pr_group3,
                pr_group4  = :pr_group4,
                pr_group5  = :pr_group5,
                pr_group1o = :pr_group1o,
                pr_group2o = :pr_group2o,
                pr_group3o = :pr_group3o,
                pr_group4o = :pr_group4o,
                pr_group5o = :pr_group5o,
                pr_ovrcreg = :pr_ovrcreg,
                pr_ovrcdiv = :pr_ovrcdiv,
                pr_costlst = :pr_costlst,
                pr_costldt = :pr_costldt,
                pr_costreg = :pr_costreg,
                pr_costdiv = :pr_costdiv,
                pr_costeac = :pr_costeac,
                pr_sales   = :pr_sales,
                pr_excuntl = :pr_excuntl,
                pr_qtyohnd = :pr_qtyohnd,
                pr_qtyoord = :pr_qtyoord,
                pr_qtyback = :pr_qtyback,
                pr_buymuli = :pr_buymuli,
                pr_buymult = :pr_buymult,
                pr_minqty  = :pr_minqty,
                pr_purincr = :pr_purincr,
                pr_disothr = :pr_disothr,
                pr_disunt7 = :pr_disunt7,
                pr_disunt8 = :pr_disunt8,
                pr_disunt9 = :pr_disunt9,
                pr_rebate  = :pr_rebate,
                pr_convpak = :pr_convpak,
                pr_weight  = :pr_weight,
                pr_weightd = :pr_weightd,
                pr_volume  = :pr_volume,
                pr_volumed = :pr_volumed,
                pr_accsale = :pr_accsale,
                pr_accouts = :pr_accouts,
                pr_accdem  = :pr_accdem,
                pr_daysout = :pr_daysout,
                pr_daysprv = :pr_daysprv,
                pr_deltcnt = :pr_deltcnt,
                pr_splttmp = :pr_splttmp,
                pr_contflg = :pr_contflg,
                pr_carcoun = :pr_carcoun,
                pr_trnoord = :pr_trnoord,
                pr_altoord = :pr_altoord,
                pr_procalt = :pr_procalt,
                pr_promqty = :pr_promqty,
                pr_prombeg = :pr_prombeg,
                pr_promend = :pr_promend,
                pr_deal    = :pr_deal,
                pr_dealbeg = :pr_dealbeg,
                pr_dealend = :pr_dealend,
                pr_dealalw = :pr_dealalw,
                pr_dealuse = :pr_dealuse,
                pr_probdat = :pr_probdat,
                pr_ansaleu = :pr_ansaleu,
                pr_ansale$ = :pr_ansale$,
                pr_rfbirth = :pr_rfbirth,
                pr_lstrcvd = :pr_lstrcvd,
                pr_leadtm  = :pr_leadtm,
                pr_leadtmv = :pr_leadtmv,
                pr_leadtms = :pr_leadtms,
                pr_forcper = :pr_forcper,
                pr_endper  = :pr_endper,
                pr_forcast = :pr_forcast,
                pr_forserr = :pr_forserr,
                pr_prvsale = :pr_prvsale,
                pr_prvlost = :pr_prvlost,
                pr_prvserr = :pr_prvserr,
                pr_prvfore = :pr_prvfore,
                pr_endpers = :pr_endpers,
                pr_svceprv = :pr_svceprv,
                pr_prvexpt = :pr_prvexpt,
                pr_prvdevp = :pr_prvdevp,
                pr_prvdemd = :pr_prvdemd,
                pr_fordevp = :pr_fordevp
                where pr_comp = :ip_comp and
                      pr_locn = :ip_locn and
                      pr_supl = :ip_supl and
                      pr_suplsub = :ip_suplsub and
                      pr_prod = :ip_prod;
       endsr;

       begsr insert_product;

       exec sql
          insert into k_product
             values(:product_rec);

       endsr;

       Begsr InzInpSrchPrdC;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pr_comp = ? and +
                     pr_locn = ? and +
                     pr_prod = ? +
                     Order by pr_comp, +
                              pr_locn, +
                              pr_prod';
       endsr;

       begsr dclprcursorc;
       exec sql
        declare prcursorc Cursor
         for DynSQLStmtC;
       endsr;

       begsr opnprcursorc;
       exec sql
        open prcursorc
          using :ip_comp,
                :ip_locn,
                :ip_prod;
       endsr;

       begsr clsprcursorc;
       exec sql
        close prcursorc;
       endsr;

       begsr PrepDynSQLStmtC;
       exec sql
        Prepare DynSqlStmtC
          From :StmtString;
       endsr;

       begsr insert_prodfor;

         exec sql
           insert into k_prodfor
             values (:prodfor_rec);

       endsr;

      /end-free

