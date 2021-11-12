*----------------------------------------------------------------------*
* Autor      : Ailson Luis                            Data: 16/08/2020 *

report zppr0099.

tables: caufv,resb,aufm.

class lcl_app definition.
  public section.

    types:
      begin of ty_alv,
        aufnr  type aufnr,
        werks  type werks_d,
        auart  type aufart,
        ernam  type ernam,
        erdat  type erdat,
        ftrmi  type caufv-ftrmi,
        idat2  type caufv-idat2,
        idat3  type caufv-idat3,
        plnbez type caufv-plnbez, "material
        ktext  type auftext,
        gamng  type gamng, "produção planejada
        aubxx  type aubxx, "Produção lançada
        gmein  type caufv-gmein,
        varprd type gamng, "variação produção
        idnrk  type idnrk,
        kmptx  type kmptx,
        bdmng  type bdmng,
        enmng  type enmng,
        meins  type meins,
        varcon type enmng, "variação consumo
        cstcpm type kkb_ml_preis, "custo do componente
        totcpm type kkb_value_total, "custo total variação de consumo
        spmon  type spmon,
      end of ty_alv,

      begin of ty_aufm,
        aufnr type aufnr,
        "bwart type bwart,
        matnr type matnr,
        qty   type menge_d,
      end of ty_aufm.






    data: rg_aufnr  type range of caufv-aufnr,
          rg_werks  type range of caufv-werks,
          rg_matnr  type range of caufv-stlbez,
          rg_dtcrea type range of caufv-erdat,
          rg_dtlib  type range of caufv-ftrmi,
          rg_dtente type range of caufv-idat2,
          rg_dtence type range of caufv-idat3,
          rg_dtlanc type range of aufm-budat.

    data: gt_alv  type table of ty_alv,
          gt_aufm type table of ty_aufm,
          gt_prod type table of ty_aufm,
          gt_cons type table of ty_aufm,
          r_alv   type ref to cl_salv_table.

    methods start.

    methods get_data.

    methods process_data.

    methods get_movements changing ch_movement type ty_alv .

    methods show_alv.


endclass.

class lcl_app implementation.


  method start.
    if rg_aufnr is initial.

      "if  rg_dtcrea is initial and rg_dtente is initial and rg_dtence is initial and rg_dtlanc is initial .
      if  rg_dtcrea is initial and rg_dtente is initial .
        message | Informe uma data como parametro de seleção! | type 'S' display like 'E'.
        leave list-processing.

      endif.
    endif.

    get_data( ).
    process_data( ).
    show_alv( ).

  endmethod.



  method get_data.
    data: rg_aufnr_aufm type range of aufnr,
          rg_mov_cons   type range of bwart,
          rg_mov_prod   type range of bwart,
          lv_qtycons    type bdmng,
          lv_qtyprod    type menge_d.

    rg_mov_cons = value #( let s = 'I' o = 'EQ' in  option = o sign = s  ( low = '261') ( low = '262' ) ( low = '543') ( low = '544') ).
    rg_mov_prod = value #( let s = 'I' o = 'EQ' in  option = o sign = s  ( low = '101') ( low = '102' ) ).

    "Seleciona as ordens de produção com componentes

    select ord~aufnr, ord~auart, ord~plnbez, ord~ktext, ord~werks, ord~ernam, ord~erdat, ord~ftrmi,  ord~idat2, ord~idat3, ord~gamng,
       ord~gmein,
       resb~rsnum, resb~matnr as idnrk, sum( abs( resb~bdmng ) ) as bdmng , resb~meins,
       makt~maktx as kmptx
      from ( caufv as ord inner join resb as resb on resb~rsnum = ord~rsnum
                          inner join makt as makt on resb~matnr = makt~matnr )
      into table @data(lt_caufv)
      where ord~aufnr in @rg_aufnr
      and ord~plnbez in @rg_matnr
      and ord~werks in @rg_werks
      and ord~erdat in @rg_dtcrea
      "and ord~ftrmi in @rg_dtlib
      and ord~idat2 in @rg_dtente
      and ord~autyp eq '40' "ORDENS DE PROCESSO
      and resb~bdmng > 0
      and makt~spras = @sy-langu
      group by ord~aufnr, ord~auart, ord~plnbez, ord~ktext, ord~werks, ord~ernam, ord~erdat, ord~ftrmi,  ord~idat2, ord~idat3, ord~gamng,
               ord~gmein,resb~rsnum, resb~matnr , resb~meins, makt~maktx .




    if lt_caufv is not initial.
      rg_aufnr_aufm = value #( for wa in lt_caufv ( option = 'EQ' sign = 'I' low = wa-aufnr ) ).

      "Qtd produzida real
      select aufnr, matnr,
        sum( case when shkzg = 'H' then menge * -1
             when shkzg = 'S' then menge
        end )  as qty
        from aufm
        into table @gt_prod
        where aufnr in @rg_aufnr
          and bwart in @rg_mov_prod
        group by aufnr, matnr.

      "Qtd consumo real
      select aufnr, matnr,
        sum( case when shkzg = 'H' then menge * -1
             when shkzg = 'S' then menge
        end )  as qty
        from aufm
        into table @gt_cons
        where aufnr in @rg_aufnr
          and bwart in @rg_mov_cons
        group by aufnr, matnr.


    endif.
    gt_alv = corresponding #(  lt_caufv ).

  endmethod.

  method process_data.
    data(vl_lines) = lines( gt_alv ).
    loop at gt_alv assigning field-symbol(<fs_alv>).
      cl_progress_indicator=>progress_indicate( i_text = | Dados de produção e consumo real da ordem { <fs_alv>-aufnr } - { sy-tabix } de { vl_lines } ordens. | i_processed = sy-tabix i_output_immediately = abap_true ).
      "producao real
      try.
        <fs_alv>-aubxx  = gt_prod[ aufnr = <fs_alv>-aufnr ]-qty.
      catch cx_sy_itab_line_not_found.

      endtry.

      "Consumo real
      try.
          <fs_alv>-enmng  = abs( gt_cons[ aufnr = <fs_alv>-aufnr matnr = <fs_alv>-idnrk ]-qty ).
      catch cx_sy_itab_line_not_found.

      endtry.


      <fs_alv>-varprd = <fs_alv>-gamng -  <fs_alv>-aubxx.

      "calculo quantidade necessária- com base na quantidade produzida
      "QtdNecessáriaReal  =  Producao Real *  consumo planejado / Producao Planejada
      <fs_alv>-bdmng = <fs_alv>-aubxx *  <fs_alv>-bdmng / <fs_alv>-gamng  .

      <fs_alv>-varcon =   <fs_alv>-enmng - <fs_alv>-bdmng.


      me->get_movements( changing ch_movement =  <fs_alv> ).
      <fs_alv>-spmon = |{ <fs_alv>-idat2(4) }{ <fs_alv>-idat2+4(2) } |.


    endloop.

  endmethod.

  method get_movements.

    data: rg_aufnr_aufm type range of aufnr,
          rg_mov_cons   type range of bwart,
          rg_mov_prod   type range of bwart,
          lv_qtycons    type bdmng,
          lv_qtyprod    type menge_d.

    rg_mov_cons = value #( let s = 'I' o = 'EQ' in  option = o sign = s  ( low = '261') ( low = '262' ) ( low = '543') ( low = '544') ).
    rg_mov_prod = value #( let s = 'I' o = 'EQ' in  option = o sign = s  ( low = '101') ( low = '102' ) ).

    "Seleciona preço do Item

    select single matnr,bwkey, lbkum, salk3, vprsv, verpr, stprs, peinh from mbewh into @data(wa_mbewh)
           where matnr eq @ch_movement-idnrk
             and bwkey eq @ch_movement-werks
             and lfgja eq @ch_movement-idat2(4)
             and lfmon eq @ch_movement-idat2+4(2).

    select single matnr,bwkey, lbkum, salk3, vprsv, verpr, stprs, peinh  from mbew
          into  @data(wa_mbew)
          where matnr eq @ch_movement-idnrk
            and bwkey eq @ch_movement-werks.


   " sort gt_aufm by matnr ascending bwart ascending.

"    clear: lv_qtycons.


"    lv_qtyprod = reduce bdmng( init x = 0 for wa_prd in gt_aufm where ( aufnr = ch_movement-aufnr and  matnr eq ch_movement-plnbez and  bwart in rg_mov_prod ) next x = x + wa_prd-qty ).
    "lv_qtycons = REDUCE BDMNG( INIT x = 0 FOR wa_cons IN gt_aufm where ( aufnr = c_movement-aufnr and  matnr eq c_movement-idnrk and  bwart in rg_mov_cons ) NEXT x = x + wa_cons-qty ).

"    loop at gt_aufm into data(wa_cons) where aufnr = ch_movement-aufnr and  matnr eq ch_movement-idnrk and  bwart in rg_mov_cons .
 "     lv_qtycons = lv_qtycons + wa_cons-qty.
  "  endloop.

  "  ch_movement-enmng  = abs( lv_qtycons ).
  "  ch_movement-aubxx  = lv_qtyprod.
  "  ch_movement-varprd = ch_movement-gamng -  ch_movement-aubxx.

    "calculo quantidade necessária- com base na quantidade produzida
    "QtdNecessáriaReal  =  Producao Real *  consumo planejado / Producao Planejada
  "  ch_movement-bdmng = ch_movement-aubxx *  ch_movement-bdmng / ch_movement-gamng  .

  "  ch_movement-varcon =   ch_movement-enmng - ch_movement-bdmng.
*
*    "Busca o preço de custo, se o preço nao existir na MBEWH para o periodo de encerramento da ordem
*    "busca do dados da MBEW-atual

    clear: ch_movement-cstcpm.

    try.
        if wa_mbewh-vprsv = 'S'.
          ch_movement-cstcpm = wa_mbewh-stprs / wa_mbewh-peinh.
        else.
          ch_movement-cstcpm = wa_mbewh-verpr / wa_mbewh-peinh.
          if ch_movement-cstcpm eq 0.
            ch_movement-cstcpm = wa_mbewh-salk3 / wa_mbewh-lbkum.
          endif.
        endif.
      catch cx_sy_itab_line_not_found.
        if wa_mbew-vprsv = 'S'.
          ch_movement-cstcpm = wa_mbew-stprs / wa_mbew-peinh.
        else.
          ch_movement-cstcpm = wa_mbew-verpr / wa_mbew-peinh.
        endif.
    endtry.

    ch_movement-totcpm = ch_movement-cstcpm * ch_movement-varcon.

    clear: lv_qtyprod, lv_qtycons.

  endmethod.


  method show_alv.
    data: lr_events     type ref to cl_salv_events_table,
          lr_selections type ref to cl_salv_selections,
          lr_columns    type ref to cl_salv_columns_table,
          lr_column     type ref to cl_salv_column.
    try.
*       Monta lista ALV de acordo com a tabela GT_CTE:
        cl_salv_table=>factory(
          exporting
            list_display   = if_salv_c_bool_sap=>false
            "r_container    =
            "container_name = 'Name'
          importing
            r_salv_table   = r_alv
          changing
            t_table        = gt_alv ).

      catch cx_salv_msg.

    endtry.

*   Funcões
    data(lr_functions) = r_alv->get_functions( ).
    lr_functions->set_all( abap_true ).

*   Seleção das linhas:
    lr_selections = r_alv->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*   Seta eventos
    "lr_events = r_alv->get_event( ).
    "set handler on_user_command for lr_events.


*   renomear colunas
    lr_columns = r_alv->get_columns( ).
    lr_columns->set_optimize( 'X' ).
    try.
        lr_column = lr_columns->get_column( 'VARPRD' ).
        lr_column->set_short_text( 'Var.Prod' ).
        lr_column->set_medium_text( 'Var.Produção' ).
        lr_column->set_long_text( 'Variação de Produção' ).

        lr_column = lr_columns->get_column( 'VARCON' ).
        lr_column->set_short_text( 'Var.Cons' ).
        lr_column->set_medium_text( 'Var.Consumo' ).
        lr_column->set_long_text( 'Variação de consumo' ).



      catch cx_salv_not_found .
        " error handling
    endtry.



*   Exibe:
    r_alv->display( ).
  endmethod.



endclass.

selection-screen begin of block bl01 with frame.

selection-screen begin of block b1 with frame title text-t01.
select-options: s_aufnr for caufv-aufnr,
                s_werks for caufv-werks,
                s_matnr for caufv-stlbez,
                s_dtcrea for caufv-erdat,
                s_dtente for caufv-idat2.


selection-screen end of block b1.

selection-screen end of block bl01.


start-of-selection.

  data r_app type ref to lcl_app.

  create object r_app.
  r_app = new lcl_app( ).
  r_app->rg_aufnr = s_aufnr[].
  r_app->rg_werks = s_werks[].
  r_app->rg_matnr = s_matnr[].
  r_app->rg_werks = s_werks[].
  r_app->rg_dtcrea = s_dtcrea[].
  r_app->rg_dtente = s_dtente[].

  r_app->start( ).
