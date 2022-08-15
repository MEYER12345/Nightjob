      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')
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
      **   Name: K3S_M090
      **   Type: ILE RPG PROGRAM
      **   Desc: Retrieve current date and time: timestamp
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/23/2014.
      **  Remarks. Converted this code from a stand-alone program to
      **           an exportable free-format RPGLE subprocedure. Added
      **           this subprocedure to service program DATEPROCS.
      **           Also, reduced the amount of code needed to get the
      **           timestamp itself by taking out the calls to API's
      **           and instead using the %Timestamp built-in function.
      *****************************************************************
     h OPTION(*NODEBUGIO) NOMAIN
      * ----------------------------------------------Parameters passed
      /copy qprotosrc
      *****************************************************************
     p K3S_M090        B                   EXPORT
     d                 PI
     d  time_stamp                     z
      *****************************************************************
      /free
       time_stamp = %Timestamp();
       return;
      /end-free
     P K3S_M090        E
