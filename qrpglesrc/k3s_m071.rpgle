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
      **   Name: K3S_M071
      **   Type: ILE RPG Program
      **   Desc: Calculate extra days to buy for alternate source
      **
      *****************************************************************
      **
      **  This program is used to calculate the extra days to buy forward
      **  for alternate source orders, and flag record that was the best
      **  price. Also, if this is a restricted quantity product, pass
      **  the restricted quantity back to the calling program.
      **
      *****************************************************************
      **
      **  Indicator usage
      **
      **      record formats
      **  11  rk_intaltr
      **
      *****************************************************************
     fk_intaltrduf   e           k disk                                         deal summary info

      * -------------------------------------------------------- work fields
     d percnt_off      s              3  1 inz(0)                               percent off
     d extra_days      s              3  0                                      extra days
     d deal            s              3  2                                      deal percent
     d days_work       s              9  0                                      days work field

      * -------------------------------------------------- parameters passed
     d comp            s                   like(ia_comp)                        buy group
     d locn            s                   like(ia_locn)                        location
     d prod            s                   like(ia_prod)                        location
     d supl            s                   like(ia_supl)                        supplier
     d suplsub         s                   like(ia_suplsub)                     sub supplier
     d cashdsc         s              3  3                                      cash discount
     d rebate          s              3  3                                      rebate
     d intrate         s              3  3                                      annual interest
     d return          s              3  3                                      return on investment
     d invmeth         s              1  0                                      investment method
     d days            s              3  0                                      extra days to buy
     d restr_qty       s              7  0                                      restricted quantity
     d last_recd       s              1  0                                      last record flag

      * -------------------------------------------------- Parameters passed
      * parameters passed to program
     c     *entry        plist                                                  parameters passed
     c                   parm                    comp                           company
     c                   parm                    locn                           location
     c                   parm                    prod                           product
     c                   parm                    supl                           supplier id
     c                   parm                    suplsub                        sub supplier id
     c                   parm                    cashdsc                        cash discount
     c                   parm                    rebate                         rebate
     c                   parm                    intrate                        interest rate
     c                   parm                    return                         return on investment
     c                   parm                    invmeth                        investment method
     c                   parm                    days                           extra days to buy
     c                   parm                    restr_qty                      restricted quantity
     c                   parm                    last_recd                      restricted quantity

      * key list for alternate source records
     c     ia_key        klist                                                  key list sel batches
     c                   kfld                    comp                            company
     c                   kfld                    locn                           company
     c                   kfld                    prod                           company

      ** read one record only, if last record time off
     c                   if        last_recd = 0

      ** initialize restricted quantity and extra days fields to be returned
     c                   eval      days = 0
     c                   eval      restr_qty = 0

     c     ia_key        setll     rk_intaltr
     c     ia_key        reade     rk_intaltr                             11

      ** if load code = 0 record exists from logical, and the record is for
      ** this alternate source, then it is the winner by price
     c                   if        *in11 = *off and
     c                             ia_suplalt = supl

     c**                 eval(h)   percnt_off = 100 -
     c**                                        (ia_costeac/ia_costreg)*100
     c**                 eval(h)   deal_days = percnt_off * 10

      ** if this product has a restricted quantity, then send it back
     c                   if        ia_restric > 0
     c                   eval      restr_qty = ia_restric

      ** not a restricted product, so calculate extra days to buy
     c                   else

      **   calculate the deal discount
     c                   eval(h)   deal = 1 - (ia_costeac/ia_costreg)

      **   calculate the number of extra days to buy
     c                   eval(h)   days_work =(( (deal - cashdsc + rebate) -
     c                                 (intrate/12) ) * 12  / return ) * 30

      **   keep from field overflow
     c                   if        days_work > 999
     c                   eval      days = 999
     c                   else
     c                   eval      days = days_work
     c                   endif

     c                   endif

     c                   eval      ia_selectd = 1
     c                   update    rk_intaltr
     c                   endif

     c                   return

      **   set on last record when k3s_1500 finished with alternate source
     c                   else
     c                   eval      *inlr = *on
     c                   endif
