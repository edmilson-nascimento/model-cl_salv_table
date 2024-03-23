
REPORT zmodel_alv.

" -----------------------------------------------------------------------
" - Tipos SAP
" -----------------------------------------------------------------------
TYPE-POOLS abap.
" -----------------------------------------------------------------------
" - Tabelas
" -----------------------------------------------------------------------
TABLES:
  zsdt016, zsdt017.

" -----------------------------------------------------------------------
" - Tipos locais
" -----------------------------------------------------------------------

" -----------------------------------------------------------------------
" - Classe
" -----------------------------------------------------------------------

CLASS zsdr0003 DEFINITION.

PUBLIC SECTION.

  TYPES:
    BEGIN OF ty_out,
      hiertype  TYPE i,               " 0=1 ATRA only, 1=sum head, 2=child ATRA
      navtree   TYPE char2,           " navigation tree, expand
      guid      TYPE /ssf/dhead-guid,
      seqno_out TYPE char5,

      gsber     TYPE zsdt016-gsber,   " DIVIS�O
      bukrs     TYPE zsdt017-bukrs,
      lifnr     TYPE zsdt017-lifnr,   " C�D CLIENTE
      xblnr     TYPE xblnr_v1,        " NF
      belnr     TYPE zsdt016-belnr,   " TITULO
      buzei     TYPE zsdt016-buzei,   " ITEM
      fkdat     TYPE zsdt016-fkdat,   " DATA EMISS�O
      bldat     TYPE bkpf-bldat,      " DATA COMPENSA��O
      kzwi1     TYPE zsdt016-kzwi1,   " VALOR LIQ PROVISIONADO
      item      TYPE zsdt017-item,
    END OF ty_out,

    tab_out TYPE TABLE OF ty_out,

    BEGIN OF ty_vbrk,
      vbeln TYPE vbrk-vbeln,
      xblnr TYPE vbrk-xblnr,
    END OF ty_vbrk,
    tab_vbrk TYPE TABLE OF ty_vbrk,

    BEGIN OF ty_j_1bnfdoc,
      docnum TYPE j_1bnfdoc-docnum,
      belnr  TYPE j_1bnfdoc-belnr,
    END OF ty_j_1bnfdoc,
    tab_j_1bnfdoc TYPE TABLE OF ty_j_1bnfdoc,

    BEGIN OF ty_j_1bnflin,
      docnum TYPE j_1bnflin-docnum,
      itmnum TYPE j_1bnflin-itmnum,
    END OF ty_j_1bnflin,
    tab_j_1bnflin TYPE TABLE OF ty_j_1bnflin.

  DATA gt_out TYPE tab_out.

  METHODS get_data
    IMPORTING !bukrs       TYPE zsdt016-bukrs
              !emissao     TYPE trg_date
              !compensacao TYPE trg_date
              !vkgrp       TYPE fip_t_vkgrp_range
              !gsber       TYPE bkk_r_gsber
              !lifnr       TYPE fip_t_bal_supplier_id_range.

  METHODS show_data.

PROTECTED SECTION.

  METHODS user_command
    FOR EVENT if_salv_events_actions_table~link_click
           OF cl_salv_events_table
    IMPORTING row
              column.

PRIVATE SECTION.

  CONSTANTS:
    empty  TYPE char2 VALUE '3',
    closed TYPE char2 VALUE '4',
    opened TYPE char2 VALUE '6'.

  DATA table TYPE REF TO cl_salv_table.

  METHODS search
    IMPORTING !bukrs       TYPE zsdt016-bukrs
              !emissao     TYPE trg_date
              !vkgrp       TYPE fip_t_vkgrp_range
              !gsber       TYPE bkk_r_gsber
              !lifnr       TYPE fip_t_bal_supplier_id_range
    EXPORTING zsdt016      TYPE zsdt016_t
              vbrk         TYPE tab_vbrk .

  METHODS organize
    IMPORTING zsdt016   TYPE zsdt016_t
              zsdt017   TYPE zsdt017_t
              vbrk      TYPE tab_vbrk
              j_1bnfdoc TYPE tab_j_1bnfdoc
              j_1bnflin TYPE tab_j_1bnflin
    EXPORTING out       TYPE tab_out.

  METHODS config_layout
    CHANGING table TYPE REF TO cl_salv_table.

  METHODS config_column
    CHANGING table TYPE REF TO cl_salv_table.

  METHODS link_click
    IMPORTING !row    TYPE any
              !column TYPE any.

ENDCLASS.


" -----------------------------------------------------------------------
" CLASS zsdr0003 IMPLEMENTATION
" -----------------------------------------------------------------------
" 
" -----------------------------------------------------------------------
CLASS zsdr0003 IMPLEMENTATION.
  METHOD get_data.
    DATA:
      lt_zsdt016   TYPE zsdt016_t,
      lt_zsdt017   TYPE zsdt017_t,
      lt_vbrk      TYPE tab_vbrk,
      lt_j_1bnfdoc TYPE tab_j_1bnfdoc,
      lt_j_1bnflin TYPE tab_j_1bnflin.

    me->search( EXPORTING bukrs       = bukrs
                          emissao     = emissao
                          compensacao = compensacao
                          vkgrp       = vkgrp
                          gsber       = gsber
                          lifnr       = lifnr
                IMPORTING zsdt016     = lt_zsdt016
                          zsdt017     = lt_zsdt017
                          vbrk        = lt_vbrk
                          j_1bnfdoc   = lt_j_1bnfdoc
                          j_1bnflin   = lt_j_1bnflin ).

    me->organize( EXPORTING zsdt016   = lt_zsdt016
                            zsdt017   = lt_zsdt017
                            vbrk      = lt_vbrk
                            j_1bnfdoc = lt_j_1bnfdoc
                            j_1bnflin = lt_j_1bnflin
                  IMPORTING out       = gt_out ).
  ENDMETHOD.

  METHOD show_data.
    DATA:
      display TYPE REF TO cl_salv_display_settings,
      events  TYPE REF TO cl_salv_events_table.

    IF gt_out IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        cl_salv_table=>factory( IMPORTING r_salv_table = table
                                CHANGING  t_table      = gt_out ).

        " Eventos do relat�rio
        events = table->get_event( ).
        SET HANDLER me->user_command FOR events.

        table->set_screen_status(                                  pfstatus      = 'STANDARD_FULLSCREEN'
                                  report        = 'SAPLKKBL'
                                  set_functions = table->c_functions_all ).

        " Configurando Layout
        me->config_layout( CHANGING table = table ).

        " Configurando Colunas
        me->config_column( CHANGING table = table ).

        " Layout de Zebra
        display = table->get_display_settings( ).
        display->set_striped_pattern( cl_salv_display_settings=>true ).

*         Ordenacao de campos
*          lo_sorts = lo_table->get_sorts( ) .
*          lo_sorts->add_sort('AS4DATE') .
*          lo_sorts->add_sort('AS4TIME') .
*          lo_sorts->add_sort('TRKORR') .

        table->display( ).

      CATCH cx_salv_msg.
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
      CATCH cx_salv_object_not_found.

    ENDTRY.
  ENDMETHOD.

  METHOD user_command.
    me->link_click( row    = row
                    column = column ).

    IF table IS BOUND.
      table->refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD search.

    DATA:
      lt_zsdt016 TYPE zsdt016_t .

    REFRESH:
      zsdt016, zsdt017, vbrk, j_1bnfdoc, j_1bnflin.

    IF bukrs IS INITIAL.
      RETURN.
    ENDIF.

    " Verificar filtro de forma que n�o user campo vazios
    SELECT * INTO TABLE zsdt016
      FROM zsdt016
      WHERE bukrs  = bukrs
        AND fkdat IN emissao
        AND gsber IN gsber
        AND vkgrp IN vkgrp
        AND lifnr IN lifnr.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    APPEND LINES OF zsdt016 TO lt_zsdt016.
    SORT lt_zsdt016 ASCENDING BY
    bukrs
    gsber
    gjahr
    belnr
    lifnr
    vbeln
    vkgrp.
    
    DELETE ADJACENT DUPLICATES FROM lt_zsdt016
           COMPARING bukrs gsber gjahr belnr lifnr vbeln vkgrp.

    APPEND LINES OF zsdt016 TO lt_zsdt016.
    SORT lt_zsdt016 ASCENDING BY vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_zsdt016
           COMPARING vbeln.

    SELECT vbeln xblnr INTO TABLE vbrk
      FROM vbrk
      FOR ALL ENTRIES IN lt_zsdt016
      WHERE vbeln = lt_zsdt016-vbeln.

    IF sy-subrc = 0.

      APPEND LINES OF vbrk TO lt_vbrk.
      SORT lt_vbrk ASCENDING BY xblnr.
      DELETE ADJACENT DUPLICATES FROM lt_vbrk
             COMPARING xblnr.

    ENDIF.
  ENDMETHOD.

  METHOD organize.
    " TODO: parameter ZSDT017 is never used (ABAP cleaner)
    " TODO: parameter VBRK is never used (ABAP cleaner)
    " TODO: parameter J_1BNFDOC is never used (ABAP cleaner)
    " TODO: parameter J_1BNFLIN is never used (ABAP cleaner)

    DATA:
      ls_zsdt016 TYPE zsdt016,
      ls_out     TYPE ty_out,
      resto      TYPE i.

    LOOP AT zsdt016 INTO ls_zsdt016.

      " resto = sy-tabix mod 2 .
      resto = 0.

      IF resto = 0.
        ls_out-navtree = me->closed.
      ELSE.
        ls_out-navtree = me->opened.
      ENDIF.

      MOVE-CORRESPONDING ls_zsdt016 TO ls_out.
      APPEND ls_out TO out.
      CLEAR  ls_out.

    ENDLOOP.
  ENDMETHOD.

  METHOD config_layout.
    DATA:
      layout TYPE REF TO cl_salv_layout,
      key    TYPE salv_s_layout_key.

    layout     = table->get_layout( ).
    key-report = sy-repid.
    layout->set_key( key ).
    layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  ENDMETHOD.

  METHOD config_column.
    DATA:
      column  TYPE REF TO cl_salv_column_list,
      columns TYPE REF TO cl_salv_columns_table.

    TRY.

        columns = table->get_columns( ).
        columns->set_optimize( 'X' ).

        column ?= columns->get_column( 'HIERTYPE' ).
        column->set_technical( if_salv_c_bool_sap=>true ).

        column ?= columns->get_column( 'NAVTREE' ).
        column->set_icon( if_salv_c_bool_sap=>true ).
        column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        column->set_long_text( 'N�vel' ).
        column->set_symbol( if_salv_c_bool_sap=>true ).

        column ?= columns->get_column( 'GUID' ).
        column->set_technical( if_salv_c_bool_sap=>true ).

        column ?= columns->get_column( 'SEQNO_OUT' ).
        column->set_technical( if_salv_c_bool_sap=>true ).

      CATCH cx_salv_not_found.

    ENDTRY.
  ENDMETHOD.

  METHOD link_click.
    " check P_SELFIELD-TABINDEX > 0.
    " if P_UCOMM = 'SELE' and P_SELFIELD-FIELDNAME = 'NAVTREE'.
    "   if LS_ALV_TRCANALYSIS-HIERTYPE = 1.
    "     if LS_ALV_TRCANALYSIS-NAVTREE = GC_ATRA_MUS_MODCLOSED.
    "       LS_ALV_TRCANALYSIS-NAVTREE = GC_ATRA_MUS_MODOPENED.
    "       LF_RTABIX = P_SELFIELD-TABINDEX + 1.
    "       loop at GT_FULLSCR_ALV_TRCANALYSES_BAK
    "       into LS_ALV_TRCANALYSIS2
    "       where GUID = LS_ALV_TRCANALYSIS-GUID and HIERTYPE = 2.
    "         insert LS_ALV_TRCANALYSIS2
    "         into GT_FULLSCR_ALV_TRCANALYSES index LF_RTABIX.
    "         add 1 to LF_RTABIX.
    "       endloop.
    "     else.
    "       LS_ALV_TRCANALYSIS-NAVTREE = GC_ATRA_MUS_MODCLOSED.
    "       delete GT_FULLSCR_ALV_TRCANALYSES
    "       where GUID = LS_ALV_TRCANALYSIS-GUID and HIERTYPE = 2.
    "     endif.
    "     modify GT_FULLSCR_ALV_TRCANALYSES from LS_ALV_TRCANALYSIS
    "     transporting NAVTREE where GUID = LS_ALV_TRCANALYSIS-GUID
    "     and HIERTYPE = 1.
    "   endif.
    "   P_SELFIELD-refresh = 'X'.
  ENDMETHOD.
ENDCLASS.

" -----------------------------------------------------------------------
" - Varaveis locais
" -----------------------------------------------------------------------
DATA obj TYPE REF TO zsdr0003.

" -----------------------------------------------------------------------
" - Tela de sele��o
" -----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.

  PARAMETERS p_bukrs TYPE zsdt016-bukrs OBLIGATORY
                                        DEFAULT '1000'.

  SELECT-OPTIONS:
    s_emiss FOR sy-datum OBLIGATORY
                         DEFAULT '20170102',
    s_compe FOR sy-datum,
    s_vkgrp FOR zsdt016-vkgrp,
    s_gsber FOR zsdt016-gsber OBLIGATORY
                              DEFAULT 'P003',
    s_lifnr FOR zsdt017-lifnr.

SELECTION-SCREEN END OF BLOCK b1.

" -----------------------------------------------------------------------
" - Eventos
" -----------------------------------------------------------------------

INITIALIZATION.

START-OF-SELECTION.
  CREATE OBJECT obj.

  obj->get_data( bukrs       = p_bukrs
                 emissao     = s_emiss[]
                 compensacao = s_compe[]
                 vkgrp       = s_vkgrp[]
                 gsber       = s_gsber[]
                 lifnr       = s_lifnr[] ).

  obj->show_data( ).