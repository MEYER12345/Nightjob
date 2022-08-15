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
      **   Name: K3S_3120
      **   Type: ILE RPG Program
      **   Desc: Get next batch number - 'Selected Products Review' or
      **                                 'Customer Usage System'
      **
      *****************************************************************

      * The purpose of this program is to get the next number to be used
      * as the batch number for a group of products in the 'Selected
      * Products Review' process, or 'Customer Usage System'.

     fk_companyauf   e           k disk
      * company values

      * -------------------------------------------------------- work fields
     d comp            s                   like(cm_comp)                        Company ID
     d batch           s              7  0                                      Batch number

      * -------------------------------------------------- Parameters passed

      * parameters passed to program
     c     *entry        plist
     c                   parm                    comp                           company
     c                   parm                    batch                          batch number

      * ---------------------------------------------------------- Key Lists

      * key list for company values
     c     cm_key        klist
     c                   kfld                    cm_comp

      * ---------------------------------------------------- Prime key lists

      *  company id
     c                   eval      cm_comp = comp

      * ---------------------------------------------- Get next batch number

      * get company record
     c     cm_key        chain     rk_company                         91

      * continue if record exists
     c                   if        NOT *in91

      * increment last batch number used
     c                   if        cm_lastba + 1 > 9999999
     c                   eval      cm_lastba = *zeros
     c                   endif
     c                   eval      cm_lastba = cm_lastba + 1

     c                   update    rk_company

      * rememeber batch number to send back as parm
     c                   eval      batch = cm_lastba

     c                   endif

      * --------------------------------------------------------------------

     c                   eval      *inlr = *on

      * --------------------------------------------------------------------
