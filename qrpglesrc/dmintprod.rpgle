       ctl-opt dftactgrp(*no);
       dcl-f k_intprod disk usage(*input);
       dcl-f kintprod disk usage(*output) rename(rk_intprod:kintprodr)
             prefix(tp:2);

       setll *start k_intprod;
       read k_intprod;
       dou %eof(k_intprod);
          exsr movefields;
          write kintprodr;
          read k_intprod;
       enddo;
       *inlr = *on;

       begsr movefields;

       tp_comp    = ip_comp;
       tp_locn    = ip_locn;
       tp_birth   = ip_birth;
       tp_lastupd = ip_lastupd;
       tp_supl    = ip_supl;
       tp_suplsub = ip_suplsub;
       tp_suplusr = ip_suplusr;
       tp_suplusb = ip_suplusb;
       tp_prod    = ip_prod;
       tp_prodseq = ip_prodseq;
       tp_desc1   = ip_desc1;
       tp_desc2   = ip_desc2;
       tp_mfg     = ip_mfg;
       tp_ndc_upc = ip_ndc_upc;
       tp_uom     = ip_uom;
       tp_packsiz = ip_packsiz;
       tp_tihi    = ip_tihi;
       tp_status  = ip_status;
       tp_minqty  = ip_minqty;
       tp_buymult = ip_buymult;
       tp_group1  = ip_group1;
       tp_group2  = ip_group2;
       tp_group3  = ip_group3;
       tp_group4  = ip_group4;
       tp_group5  = ip_group5;
       tp_whslocn = ip_whslocn;
       tp_costreg = ip_costreg;
       tp_costdiv = ip_costdiv;
       tp_sales   = ip_sales;
       tp_saleslw = ip_saleslw;
       tp_qtyohnd = ip_qtyohnd;
       tp_qtyoord = ip_qtyoord;
       tp_qtyback = ip_qtyback;
       tp_weight  = ip_weight;
       tp_weightd = ip_weightd;
       tp_volume  = *zeros;
       tp_volumed = ip_volumed;
       tp_dlysale = ip_dlysale;
       tp_dlyouts = ip_dlyouts;
       tp_trnoord = ip_trnoord;
       tp_altoord = ip_altoord;
       tp_forctyp = ip_forctyp;
       tp_convpak = ip_convpak;
       tp_purincr = ip_purincr;
       tp_contflg = ip_contflg;
       tp_rebate  = ip_rebate;
       tp_procalt = ip_procalt;
       tp_mfgout  = ip_mfgout;
       tp_dlytyp1 = ip_dlytyp1;
       tp_dlytyp2 = ip_dlytyp2;
       tp_dlytyp3 = ip_dlytyp3;
       tp_dlytyp4 = ip_dlytyp4;
       tp_dlytyp5 = ip_dlytyp5;
       tp_dlytyp6 = ip_dlytyp6;
       tp_dlytyp7 = ip_dlytyp7;
       tp_dlytyp8 = ip_dlytyp8;
       tp_dlytyp9 = ip_dlytyp9;
       tp_carcoun = ip_carcoun;
       tp_usera1  = ip_usera1;
       tp_usera2  = ip_usera2;
       tp_usera3  = ip_usera3;
       tp_usern1  = ip_usern1;
       tp_usern2  = ip_usern2;
       tp_usern3  = ip_usern3;
       tp_disothr = ip_disothr;
       tp_disunt7 = ip_disunt7;
       tp_disunt8 = ip_disunt8;
       tp_disunt9 = ip_disunt9;

       endsr;
