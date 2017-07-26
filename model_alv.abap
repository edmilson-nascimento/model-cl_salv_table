
report  zmodel_alv.

*----------------------------------------------------------------------*
*- Tipos SAP
*----------------------------------------------------------------------*
type-pools:
  abap .
*----------------------------------------------------------------------*
*- Tabelas
*----------------------------------------------------------------------*
tables:
  zsdt016, zsdt017 .

*----------------------------------------------------------------------*
*- Tipos locais
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*- Classe
*----------------------------------------------------------------------*

class zsdr0003 definition .

  public section .

    types:
    begin of ty_out,
      hiertype  type i, "0=1 ATRA only, 1=sum head, 2=child ATRA
      navtree   type char2, "navigation tree, expand
      guid      type /ssf/dhead-guid,
      seqno_out type char5,

      gsber type zsdt016-gsber, " DIVISÃO
      " CÓD ERC
      " NOME ERC
      bukrs type zsdt017-bukrs,
      lifnr type zsdt017-lifnr, " CÓD CLIENTE
      xblnr type xblnr_v1," NF
      belnr type zsdt016-belnr, " TITULO
      buzei type zsdt016-buzei, " ITEM
      " VALOR LIQ ITEM
      fkdat type zsdt016-fkdat, " DATA EMISSÃO
      bldat type bkpf-bldat, " DATA COMPENSAÇÃO
      kzwi1 type zsdt016-kzwi1, " VALOR LIQ PROVISIONADO
      " % COMISSÃO FATURADO
      " VALOR LIQ COMPENSADO
      " VL COMISSÃO REPROCESSADO
      " % COMISSÃO REPROCESSADO
      item      type zsdt017-item,
    end of ty_out,

    tab_out type table of ty_out,

    begin of ty_vbrk,
      vbeln type vbrk-vbeln,
      xblnr type vbrk-xblnr,
    end of ty_vbrk,
    tab_vbrk type table of ty_vbrk,

    begin of ty_j_1bnfdoc,
      docnum type j_1bnfdoc-docnum,
      belnr  type j_1bnfdoc-belnr,
    end of ty_j_1bnfdoc,
    tab_j_1bnfdoc type table of ty_j_1bnfdoc,

    begin of ty_j_1bnflin,
      docnum type j_1bnflin-docnum,
      itmnum type j_1bnflin-itmnum,
    end of ty_j_1bnflin,
    tab_j_1bnflin type table of ty_j_1bnflin .

    data:
      gt_out type tab_out .

    methods get_data
     importing
       !bukrs       type zsdt016-bukrs
       !emissao     type trg_date
       !compensacao type trg_date
       !vkgrp       type fip_t_vkgrp_range
       !gsber       type bkk_r_gsber
       !lifnr       type fip_t_bal_supplier_id_range .

    methods show_data .

  protected section .

    methods user_command
      for event if_salv_events_actions_table~link_click
             of cl_salv_events_table
      importing row
                column .

  private section .

    constants:
      empty  type char2 value '3',
      closed type char2 value '4',
      opened type char2 value '6' .

    data:
      table type ref to cl_salv_table .

    methods search
      importing
        !bukrs       type zsdt016-bukrs
        !emissao     type trg_date
        !compensacao type trg_date
        !vkgrp       type fip_t_vkgrp_range
        !gsber       type bkk_r_gsber
        !lifnr       type fip_t_bal_supplier_id_range
      exporting
        zsdt016      type zsdt016_t
        zsdt017      type zsdt017_t
        vbrk         type tab_vbrk
        j_1bnfdoc    type tab_j_1bnfdoc
        j_1bnflin    type tab_j_1bnflin .

    methods organize
      importing
        zsdt016      type zsdt016_t
        zsdt017      type zsdt017_t
        vbrk         type tab_vbrk
        j_1bnfdoc    type tab_j_1bnfdoc
        j_1bnflin    type tab_j_1bnflin
      exporting
        out          type tab_out .

    methods config_layout
      changing table type ref to cl_salv_table .

    methods config_column
      changing table type ref to cl_salv_table .

    methods link_click
      importing
        !row    type any
        !column type any .

endclass .                    "zsdr0003 DEFINITION

*----------------------------------------------------------------------*
*       CLASS zsdr0003 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class zsdr0003 implementation .

  method get_data .

    data:
      lt_zsdt016   type zsdt016_t,
      lt_zsdt017   type zsdt017_t,
      lt_vbrk      type tab_vbrk,
      lt_j_1bnfdoc type tab_j_1bnfdoc,
      lt_j_1bnflin type tab_j_1bnflin .

    me->search(
      exporting
        bukrs       = bukrs
        emissao     = emissao
        compensacao = compensacao
        vkgrp       = vkgrp
        gsber       = gsber
        lifnr       = lifnr
      importing
        zsdt016     = lt_zsdt016
        zsdt017     = lt_zsdt017
        vbrk        = lt_vbrk
        j_1bnfdoc   = lt_j_1bnfdoc
        j_1bnflin   = lt_j_1bnflin
     ) .

    me->organize(
      exporting
        zsdt016     = lt_zsdt016
        zsdt017     = lt_zsdt017
        vbrk        = lt_vbrk
        j_1bnfdoc   = lt_j_1bnfdoc
        j_1bnflin   = lt_j_1bnflin
      importing
        out         = gt_out
    ).

  endmethod .                    "get_data

  method show_data .

    data:
      display type ref to cl_salv_display_settings,
      events  type ref to cl_salv_events_table .

    if gt_out is not initial .

      try.

          call method cl_salv_table=>factory
            IMPORTING
              r_salv_table = table
            CHANGING
              t_table      = gt_out.

*         Eventos do relatório
          events = table->get_event( ).
          set handler me->user_command for events.

          table->set_screen_status(
*         pfstatus      = 'ZTESTE'
          pfstatus      = 'STANDARD_FULLSCREEN'
*         report        = sy-cprog
          report        = 'SAPLKKBL'
          set_functions = table->c_functions_all ).

*         Configurando Layout
          me->config_layout(
            changing
              table = table
          ) .

*         Configurando Colunas
          me->config_column(
            changing
              table = table
          ) .

*         Layout de Zebra
          display = table->get_display_settings( ) .
          display->set_striped_pattern( cl_salv_display_settings=>true ) .

*         Ordenação de campos
*          lo_sorts = lo_table->get_sorts( ) .
*          lo_sorts->add_sort('AS4DATE') .
*          lo_sorts->add_sort('AS4TIME') .
*          lo_sorts->add_sort('TRKORR') .

          table->display( ).

        catch cx_salv_msg .
        catch cx_salv_not_found .
        catch cx_salv_existing .
        catch cx_salv_data_error .
        catch cx_salv_object_not_found .

      endtry.

    endif .


  endmethod .                    "show_data

  method user_command .

    me->link_click(
      exporting
        row    = row
        column = column
    ).

    if table is bound .
      table->refresh( ) .
    endif .

  endmethod .                    "user_command

  method search .

    data:
      lt_zsdt016 type zsdt016_t,
      lt_vbrk    type tab_vbrk .

    refresh:
      zsdt016, zsdt017, vbrk, j_1bnfdoc, j_1bnflin .

    if bukrs is not initial .

*     Verificar filtro de forma que não user campo vazios
      select *
        into table zsdt016
        from zsdt016
       where bukrs eq bukrs
         and fkdat in emissao
         and gsber in gsber
         and vkgrp in vkgrp
         and lifnr in lifnr .

      if sy-subrc eq 0 .

        append lines of zsdt016 to lt_zsdt016 .
        sort lt_zsdt016 ascending by
        bukrs gsber gjahr belnr lifnr vbeln vkgrp .
        delete adjacent duplicates from lt_zsdt016
        comparing bukrs gsber gjahr belnr lifnr vbeln vkgrp .

*        select *
*          into table zsdt017
*          from zsdt017
*           for all entries in lt_zsdt016
*         where bukrs eq lt_zsdt016-bukrs
*           and gsber eq lt_zsdt016-gsber
*           and gjahr eq lt_zsdt016-gjahr
*           and belnr eq lt_zsdt016-belnr
*           and lifnr eq lt_zsdt016-lifnr
*           and vbeln eq lt_zsdt016-vbeln
*           and vkgrp eq lt_zsdt016-vkgrp .

        append lines of zsdt016 to lt_zsdt016 .
        sort lt_zsdt016 ascending by vbeln .
        delete adjacent duplicates from lt_zsdt016
        comparing vbeln .

        select vbeln xblnr
          into table vbrk
          from vbrk
           for all entries in lt_zsdt016
         where vbeln eq lt_zsdt016-vbeln .

        if sy-subrc eq 0 .

          append lines of vbrk to lt_vbrk .
          sort lt_vbrk ascending by xblnr .
          delete adjacent duplicates from lt_vbrk
          comparing xblnr .

*          select docnum
*            into table j_1bnfdoc
*            from j_1bnfdoc
*             for all entries in lt_vbrk
*           where docnum eq lt_vbrk-xblnr .

        endif .

      endif .

    endif .

  endmethod .                    "search

  method organize .

    data:
      ls_zsdt016 type zsdt016,
      ls_out     type ty_out,
      resto      type i .

    loop at zsdt016 into ls_zsdt016 .

*     resto = sy-tabix mod 2 .
      resto = 0 .

      if resto eq 0 .
        ls_out-navtree = me->closed .
      else .
        ls_out-navtree = me->opened .
      endif .

      move-corresponding ls_zsdt016 to ls_out .
      append ls_out to out .
      clear  ls_out .

    endloop .

  endmethod .                    "organize

  method config_layout .

    data:
      layout type ref to cl_salv_layout,
      key    type salv_s_layout_key .

    layout     = table->get_layout( ).
    key-report = sy-repid.
    layout->set_key( key ).
    layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  endmethod .                    "config_layout

  method config_column .

    data:
      column  type ref to cl_salv_column_list,
      columns type ref to cl_salv_columns_table .

    try .

        columns = table->get_columns( ).
        columns->set_optimize( 'X' ).

        column ?= columns->get_column( 'HIERTYPE' ).
        column->set_technical( if_salv_c_bool_sap=>true ) .

        column ?= columns->get_column( 'NAVTREE' ).
        column->set_icon( if_salv_c_bool_sap=>true ).
        column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        column->set_long_text( 'Nível' ).
        column->set_symbol( if_salv_c_bool_sap=>true ).

        column ?= columns->get_column( 'GUID' ).
        column->set_technical( if_salv_c_bool_sap=>true ) .

        column ?= columns->get_column( 'SEQNO_OUT' ).
        column->set_technical( if_salv_c_bool_sap=>true ) .

      catch cx_salv_not_found .

    endtry .

  endmethod .                    "config_column

  method link_click .

*    check P_SELFIELD-TABINDEX > 0.
*    if P_UCOMM = 'SELE' and P_SELFIELD-FIELDNAME = 'NAVTREE'.
*      if LS_ALV_TRCANALYSIS-HIERTYPE = 1.
*        if LS_ALV_TRCANALYSIS-NAVTREE = GC_ATRA_MUS_MODCLOSED.
*          LS_ALV_TRCANALYSIS-NAVTREE = GC_ATRA_MUS_MODOPENED.
*          LF_RTABIX = P_SELFIELD-TABINDEX + 1.
*          loop at GT_FULLSCR_ALV_TRCANALYSES_BAK
*          into LS_ALV_TRCANALYSIS2
*          where GUID = LS_ALV_TRCANALYSIS-GUID and HIERTYPE = 2.
*            insert LS_ALV_TRCANALYSIS2
*            into GT_FULLSCR_ALV_TRCANALYSES index LF_RTABIX.
*            add 1 to LF_RTABIX.
*          endloop.
*        else.
*          LS_ALV_TRCANALYSIS-NAVTREE = GC_ATRA_MUS_MODCLOSED.
*          delete GT_FULLSCR_ALV_TRCANALYSES
*          where GUID = LS_ALV_TRCANALYSIS-GUID and HIERTYPE = 2.
*        endif.
*        modify GT_FULLSCR_ALV_TRCANALYSES from LS_ALV_TRCANALYSIS
*        transporting NAVTREE where GUID = LS_ALV_TRCANALYSIS-GUID
*        and HIERTYPE = 1.
*      endif.
*      P_SELFIELD-refresh = 'X'.

  endmethod .                    "link_click

endclass .                    "zsdr0003 IMPLEMENTATION

*----------------------------------------------------------------------*
*- Varaveis locais
*----------------------------------------------------------------------*
data:
  obj type ref to zsdr0003 .

*----------------------------------------------------------------------*
*- Tela de seleção
*----------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-t01.

parameters:
  p_bukrs type zsdt016-bukrs obligatory
                             default '1000' .

select-options:
  s_emiss for sy-datum obligatory
                       default '20170102',
  s_compe for sy-datum,
  s_vkgrp for zsdt016-vkgrp,
  s_gsber for zsdt016-gsber obligatory
                            default 'P003',
  s_lifnr for zsdt017-lifnr.

selection-screen end of block b1.

*----------------------------------------------------------------------*
*- Eventos
*----------------------------------------------------------------------*

initialization .

start-of-selection .

  create object obj .

  obj->get_data(
    exporting
      bukrs       = p_bukrs
      emissao     = s_emiss[]
      compensacao = s_compe[]
      vkgrp       = s_vkgrp[]
      gsber       = s_gsber[]
      lifnr       = s_lifnr[]
    ) .

  obj->show_data( ) .