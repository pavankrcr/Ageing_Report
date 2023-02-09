REPORT  customerageingreport.

TYPE-POOLS :slis.
TABLES :t001, bsid ,knvp,faglflexa,t005t,t005u,vbak,kna1.

TYPES : BEGIN OF st_bsid,
          bukrs TYPE bkpf-bukrs,
          kunnr TYPE vbak-kunnr,
          belnr TYPE bkpf-belnr,
          shkzg TYPE bsid-shkzg,
          dmbtr TYPE bsid-dmbtr,
          budat TYPE bsid-budat,
          bldat TYPE bsid-bldat,
          waers TYPE bsid-waers,
          xblnr TYPE bsid-xblnr,
          blart TYPE bsid-blart,
          wrbtr TYPE bsid-wrbtr,
          saknr TYPE bsid-saknr,
          prctr TYPE bsid-prctr,
          zfbdt TYPE bsid-zfbdt,
          zterm TYPE bsid-zterm,
          zbd1t TYPE bsid-zbd1t,
          augdt TYPE bsid-augdt,
          gjahr TYPE bsid-gjahr,
          umskz TYPE bsid-umskz,
          txt50 TYPE skat-txt50,
        END OF st_bsid,

        BEGIN OF st_final,
          bukrs         TYPE bkpf-bukrs,
          kunnr         TYPE vbak-kunnr,
          belnr         TYPE bkpf-belnr,
          shkzg         TYPE bsid-shkzg,
          dmbtr         TYPE bsid-dmbtr,
          budat         TYPE bsid-budat,
          bldat         TYPE bsid-bldat,
          waers         TYPE bsid-waers,
          xblnr         TYPE bsid-xblnr,
          blart         TYPE bsid-blart,
          wrbtr         TYPE bsid-wrbtr,
          saknr         TYPE bsid-saknr,
          prctr         TYPE bsid-prctr,
          zfbdt         TYPE bsid-zfbdt,
          zterm         TYPE bsid-zterm,
          zbd1t         TYPE bsid-zbd1t,
          augdt         TYPE bsid-augdt,
          notdue        TYPE bsid-dmbtr,
          due30         TYPE bsid-dmbtr,
          due60         TYPE bsid-dmbtr,
          due90         TYPE bsid-dmbtr,
          due120        TYPE bsid-dmbtr,
          due150        TYPE bsid-dmbtr,
          due180        TYPE bsid-dmbtr,
          due365        TYPE bsid-dmbtr,
          duelast       TYPE bsid-dmbtr,
          name1         TYPE kna1-name1,
          werks         TYPE bseg-werks,
          ktokd         TYPE kna1-ktokd,
          re_inst_value TYPE bsid-dmbtr,
          umskz         TYPE bsid-umskz,
          txt50         TYPE skat-txt50,
          ukurs         TYPE tcurr-ukurs,
          bookvalue     TYPE bsid-dmbtr,
        END OF st_final,

        BEGIN OF st_kna1,
          kunnr TYPE kna1-kunnr,
          name1 TYPE kna1-name1,
          ktokd TYPE kna1-ktokd,
        END OF st_kna1,

        BEGIN OF st_faglflexa,
          ryear  TYPE faglflexa-prctr,
          belnr  TYPE bsid-belnr,
          prctr  TYPE faglflexa-prctr,
          rbukrs TYPE faglflexa-rbukrs,
        END OF st_faglflexa,

        BEGIN OF st_tcurr,
          kurst TYPE tcurr-kurst,
          fcurr TYPE tcurr-fcurr,
          tcurr TYPE tcurr-tcurr,
          gdatu TYPE tcurr-gdatu,
          ukurs TYPE tcurr-ukurs,
        END OF st_tcurr,

        BEGIN OF st_skat,
          saknr TYPE skat-saknr,
          txt50 TYPE skat-txt50,
        END OF st_skat.

DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      wa_layout   TYPE slis_layout_alv.

DATA: it_bsid      TYPE TABLE OF st_bsid,
      it_final     TYPE TABLE OF st_final,
      it_kna1      TYPE TABLE OF st_kna1,
      wa_kna1      TYPE st_kna1,
      wa_bsid      TYPE st_bsid,
      wa_faglflexa TYPE st_faglflexa,
      wa_final     TYPE st_final,
      wa_tcurr     TYPE st_tcurr,
      wa_skat      TYPE st_skat,
      wa1_final    TYPE st_final,
      wa2_final    TYPE st_final.


DATA: days TYPE i,
      usd  TYPE f,
      gbp  TYPE f,
      euro TYPE f.



SELECTION-SCREEN BEGIN OF BLOCK blk.

SELECT-OPTIONS :  s_bukrs  FOR  t001-bukrs OBLIGATORY NO-EXTENSION NO INTERVALS.

PARAMETERS     :  p_date  LIKE bsid-budat DEFAULT sy-datum OBLIGATORY.

SELECT-OPTIONS :  s_pernr FOR  knvp-pernr.

SELECT-OPTIONS :  s_kunnr  FOR  bsid-kunnr OBLIGATORY,
                  s_prctr  FOR  faglflexa-prctr.

SELECT-OPTIONS : s_land1 FOR t005t-land1,
                 s_bezei FOR t005u-bezei.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(15) TEXT-018 .

SELECTION-SCREEN POSITION POS_LOW .
PARAMETERS : rastbis1 LIKE  rfpdo1-allgrogr DEFAULT 030,
             rastbis2 LIKE  rfpdo1-allgrogr  DEFAULT 060,
             rastbis3 LIKE  rfpdo1-allgrogr  DEFAULT 090,
             rastbis4 LIKE  rfpdo1-allgrogr  DEFAULT 120,
             rastbis5 LIKE  rfpdo1-allgrogr  DEFAULT 180,
             rastbis6 LIKE  rfpdo1-allgrogr  DEFAULT 365.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF BLOCK rb WITH FRAME .
PARAMETERS: r_inv RADIOBUTTON GROUP rad,
            r_due RADIOBUTTON GROUP rad.

SELECTION-SCREEN END OF BLOCK rb.
SELECTION-SCREEN END OF BLOCK blk.

RANGES r_fiscal FOR bkpf-gjahr.

DATA: wa_pernr LIKE LINE OF s_pernr.

START-OF-SELECTION.

  PERFORM getdata.

  PERFORM processdata.

  PERFORM net_invoice_against_payment.

  PERFORM net_non_cleared_advances.

  PERFORM calculate_ageing.

  PERFORM display.


FORM getdata.
  SELECT bukrs,
         kunnr,
         belnr,
         shkzg,
         dmbtr,
         budat,
         bldat,
         waers,
         xblnr,
         blart,
         wrbtr,
         saknr,
         prctr,
         zfbdt,
         zterm,
         zbd1t,
         augdt,
         gjahr,
         umskz
    FROM bsid WHERE bukrs = @s_bukrs-low AND kunnr IN @s_kunnr AND budat <= @p_date INTO TABLE @it_bsid.
  IF ( sy-subrc = 0 ).
    SELECT bukrs,
         kunnr,
         belnr,
         shkzg,
         dmbtr,
         budat,
         bldat,
         waers,
         xblnr,
         blart,
         wrbtr,
         saknr,
         prctr,
         zfbdt,
         zterm,
         zbd1t,
         augdt,
         gjahr,
         umskz
    FROM bsad WHERE bukrs = @s_bukrs-low AND kunnr IN @s_kunnr AND budat <= @p_date AND augdt > @p_date INTO TABLE @DATA(it_bsad).
    IF ( sy-subrc = 0 ).
      APPEND LINES OF it_bsad TO it_bsid .
    ENDIF.
  ENDIF.

ENDFORM.

FORM processdata.
  DATA curr_day TYPE d.
  LOOP AT it_bsid INTO wa_bsid.

    IF wa_bsid-shkzg = 'H'.
      wa_bsid-dmbtr =  wa_bsid-dmbtr * -1.
      wa_bsid-wrbtr =  wa_bsid-wrbtr * -1.
    ENDIF.
    IF ( wa_bsid-shkzg <> 'H' ).
      curr_day = p_date - 31.

      SELECT SINGLE kurst,fcurr,tcurr,gdatu,ukurs FROM tcurr WHERE kurst = 'M' AND tcurr = 'INR' AND fcurr = @wa_bsid-waers AND ( gdatu = @p_date OR gdatu >= @curr_day ) INTO @wa_tcurr.

      wa_final-re_inst_value = wa_bsid-wrbtr * wa_tcurr-ukurs.
    ENDIF.

    SELECT SINGLE kunnr,name1,ktokd FROM kna1 WHERE kunnr = @wa_bsid-kunnr INTO @wa_kna1.

    SELECT SINGLE ryear,belnr,prctr,rbukrs FROM faglflexa WHERE ryear = @wa_bsid-gjahr AND belnr = @wa_bsid-belnr AND rbukrs = @wa_bsid-bukrs INTO @wa_faglflexa.

    SELECT SINGLE saknr,txt50  FROM skat WHERE saknr = @wa_bsid-saknr INTO @wa_skat.

    wa_final-prctr = wa_faglflexa-prctr.
    wa_final-name1 = wa_kna1-name1.
    wa_final-ktokd = wa_kna1-ktokd.
    wa_final-txt50 = wa_skat-txt50.
    wa_final-ukurs  = wa_tcurr-ukurs.
    wa_bsid-zfbdt = wa_bsid-zfbdt + wa_bsid-zbd1t.

    wa_final-wrbtr   = wa_bsid-wrbtr.

    wa_final-kunnr = wa_bsid-kunnr.

    wa_final-bukrs = wa_bsid-bukrs.

    wa_final-belnr  = wa_bsid-belnr.

    wa_final-shkzg  = wa_bsid-shkzg.

    wa_final-dmbtr  = wa_bsid-dmbtr.

    wa_final-budat  = wa_bsid-budat.

    wa_final-bldat = wa_bsid-bldat.

    wa_final-waers  = wa_bsid-waers.

    wa_final-xblnr  = wa_bsid-xblnr .

    wa_final-blart   = wa_bsid-blart.

    wa_final-saknr   = wa_bsid-saknr.

    wa_final-zfbdt   = wa_bsid-zfbdt.

    wa_final-zterm   = wa_bsid-zterm.

    wa_final-zbd1t   = wa_bsid-zbd1t.

    wa_final-umskz   = wa_bsid-umskz.

    SHIFT wa_bsid-kunnr LEFT DELETING LEADING '0'.

    SHIFT wa_bsid-saknr LEFT DELETING LEADING '0'.

    APPEND wa_final TO it_final.

    CLEAR wa_final.

    CLEAR wa_tcurr-ukurs.
  ENDLOOP.

ENDFORM.

FORM net_invoice_against_payment.
  SORT it_final BY kunnr xblnr budat bldat.
  LOOP AT it_final INTO wa1_final .
    READ TABLE it_final INDEX ( sy-tabix + 1 ) INTO wa2_final.
    IF ( wa1_final-kunnr = wa2_final-kunnr  AND  wa1_final-xblnr = wa2_final-xblnr  AND ( wa1_final-blart = 'RV' OR wa1_final-blart = 'DR' ) AND  wa2_final-shkzg ='H' ).
      wa1_final-dmbtr = wa1_final-dmbtr + wa2_final-dmbtr.
      wa1_final-wrbtr = wa1_final-wrbtr + wa2_final-wrbtr.
      MODIFY it_final FROM wa1_final INDEX ( sy-tabix - 1 ).
      DELETE it_final INDEX ( sy-tabix ) .
    ENDIF.
    CLEAR wa1_final .
    CLEAR wa2_final.
  ENDLOOP.
ENDFORM.

FORM net_non_cleared_advances.
  IF ( r_due = 'X' ).

    SORT it_final BY kunnr shkzg  zfbdt .
    LOOP AT it_final INTO wa1_final .
      READ TABLE it_final INDEX ( sy-tabix + 1 ) INTO wa2_final.
      IF ( wa1_final-kunnr = wa2_final-kunnr  AND wa1_final-shkzg ='H' ).
        wa2_final-dmbtr = wa2_final-dmbtr + wa1_final-dmbtr.
        wa2_final-re_inst_value = wa2_final-re_inst_value + wa1_final-re_inst_value.
        wa2_final-wrbtr = wa2_final-wrbtr + wa1_final-wrbtr.
        IF ( wa2_final-dmbtr <= 0 ).
          wa2_final-shkzg = 'H' .
        ENDIF.
        MODIFY it_final FROM wa2_final INDEX ( sy-tabix  ).
        DELETE it_final INDEX ( sy-tabix - 1 ) .
      ENDIF.
      CLEAR wa1_final .
      CLEAR wa2_final.
    ENDLOOP.
  ENDIF.
ENDFORM.


FORM calculate_ageing.

  LOOP AT it_final INTO wa_final.

    days = p_date - wa_final-zfbdt.
    wa_final-bookvalue = wa_final-dmbtr.

    IF ( wa_final-waers <> 'INR' AND wa_final-re_inst_value <> 0 ).
      wa_final-dmbtr = wa_final-re_inst_value .
    ENDIF.

    IF ( wa_final-waers = 'INR' ).
      wa_final-wrbtr = 0 .
    ENDIF.


    IF days < 0.
      wa_final-notdue = wa_final-dmbtr.
    ENDIF.

    IF days >= 0 AND  days <= 30 .
      wa_final-due30 = wa_final-dmbtr.
    ENDIF.

    IF days > 30 AND  days <= 60 .
      wa_final-due60 = wa_final-dmbtr.
    ENDIF.

    IF days > 60 AND  days <= 90 .
      wa_final-due90 = wa_final-dmbtr.
    ENDIF.

    IF days > 90 AND  days <= 120 .
      wa_final-due120 = wa_final-dmbtr.
    ENDIF.

    IF days > 120 AND  days <= 150 .
      wa_final-due150 = wa_final-dmbtr.
    ENDIF.

    IF days > 150 AND  days <= 180 .
      wa_final-due180 = wa_final-dmbtr.
    ENDIF.

    IF days > 180 AND  days <= 365 .
      wa_final-due365 = wa_final-dmbtr.
    ENDIF.

    IF days > 365.
      wa_final-duelast = wa_final-dmbtr.
    ENDIF.
    MODIFY it_final FROM wa_final.
  ENDLOOP.
ENDFORM.

FORM display.
  wa_fieldcat-fieldname  = 'BUKRS'.
  wa_fieldcat-seltext_m  = 'Company Code'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'KUNNR'.
  wa_fieldcat-seltext_m  = 'Customer Code'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'BELNR'.
  wa_fieldcat-seltext_m  = 'Document No'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'SHKZG'.
  wa_fieldcat-seltext_m  = 'Posting Key'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'BOOKVALUE'.
  wa_fieldcat-seltext_m  = 'Amount in Local Currency'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'BUDAT'.
  wa_fieldcat-seltext_m  = 'Posting Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'BLDAT'.
  wa_fieldcat-seltext_m  = 'Document Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'WAERS'.
  wa_fieldcat-seltext_m  = 'Currency'.
  APPEND wa_fieldcat TO it_fieldcat.


  wa_fieldcat-fieldname  = 'XBLNR'.
  wa_fieldcat-seltext_m  = 'Reference'.
  APPEND wa_fieldcat TO it_fieldcat.


  wa_fieldcat-fieldname  = 'BLART'.
  wa_fieldcat-seltext_m  = 'Document Type'.
  APPEND wa_fieldcat TO it_fieldcat.


  wa_fieldcat-fieldname  = 'WRBTR'.
  wa_fieldcat-seltext_m  = 'Amount FCR'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'UKURS'.
  wa_fieldcat-seltext_m  = 'Exhange Rate'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'RE_INST_VALUE'.
  wa_fieldcat-seltext_m  = 'Re-Instated Value'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DMBTR'.
  wa_fieldcat-seltext_m  = 'Ageing Amount'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.


  wa_fieldcat-fieldname  = 'SAKNR'.
  wa_fieldcat-seltext_m  = 'G/L Account'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'TXT50'.
  wa_fieldcat-seltext_m  = 'G/L Account Long Text'.
  APPEND wa_fieldcat TO it_fieldcat.


  wa_fieldcat-fieldname  = 'PRCTR'.
  wa_fieldcat-seltext_m  = 'Profit Center'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'ZFBDT'.
  wa_fieldcat-seltext_m  = 'Due Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'ZTERM '.
  wa_fieldcat-seltext_m  = 'Payt Terms'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'ZBD1T'.
  wa_fieldcat-seltext_m  = 'Payt Terms Days'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'NOTDUE'.
  wa_fieldcat-seltext_m  = 'Not Due'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DUE30'.
  wa_fieldcat-seltext_m  = '0-30'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DUE60'.
  wa_fieldcat-seltext_m  = '31-60'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DUE90'.
  wa_fieldcat-seltext_m  = '61-90'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DUE120'.
  wa_fieldcat-seltext_m  = '91-120'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DUE150'.
  wa_fieldcat-seltext_m  = '121-150'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DUE180'.
  wa_fieldcat-seltext_m  = '151-180'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DUE365'.
  wa_fieldcat-seltext_m  = '181-365'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'DUELAST'.
  wa_fieldcat-seltext_m  = '>365'.
  wa_fieldcat-do_sum     = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'NAME1'.
  wa_fieldcat-seltext_m  = 'Customer Name'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'KTOKD'.
  wa_fieldcat-seltext_m  = 'Customer Group'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-fieldname  = 'UMSKZ'.
  wa_fieldcat-seltext_m  = 'Special G/L ind'.
  APPEND wa_fieldcat TO it_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_save             = 'X'
      it_fieldcat        = it_fieldcat
    TABLES
      t_outtab           = it_final
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.


ENDFORM.
