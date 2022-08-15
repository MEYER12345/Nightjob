      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h OPTION(*NODEBUGIO)

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
      **   Name: K3S_7000
      **   Type: ILE RPG Program
      **   Desc: Get next purchase order number for creation
      **
      *****************************************************************

      * This program will pass back the first in a series of deal numbers
      * to be used for a new deal. If only one location is being processed,
      * then only one deal number is used. However, if multiple locations
      * are being processed (how_many > 0), then the first_deal # passed
      * back will actually be used as both the deal reference ID, and
      * the deal # for the first valid location being processed.
     * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- parameter passed prototype
     d*K3S_7000        PR
     d* comp                          1
     d* first_po#                    10
     d*
     d K3S_7000        PI
     d  comp                          1
     d  first_po#                    10
     d*
     d company_rec   e ds                  ExtName(k_company)
     d*
     d SqlStateOk      c                   Const('00000')
     d*
      /free
       exec sql
        set option commit = *none;
       //get company record
       Exec sql
        Select *
          into :company_rec
          from k_companya
          where cm_comp = :comp
          fetch first row only;

       //continue if record exists
       if SQLSTT = SqlStateOk;

       //increment last deal# used
         if        cm_lastpo = 9999999;
                   cm_lastpo = *zeros;
         endif;
         cm_lastpo += 1;

       //remember po#
         Exec sql
          update k_company
          set cm_lastpo = :cm_lastpo
          where cm_comp = :comp;

         first_po# = %editc(cm_lastpo:'X');

       endif;

       //-------------------------------------------------------------------

       *inlr = *on;
      /end-free
