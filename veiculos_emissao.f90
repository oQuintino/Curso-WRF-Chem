program veiculos_emissao
    implicit none

    ! Declaração das variáveis do namelist
    integer :: n_carro, n_moto, n_onibus, n_caminhao

    ! OBS: os fatores de emissão são conjuntos de três valores para CO2, CO e CH4
    real :: aumento_veiculos, &
    fator_emissao_carro(3), fator_emissao_moto(3), &
    fator_emissao_onibus(3), fator_emissao_caminhao(3), &
    ajuste_emissao_carro, ajuste_emissao_moto, &
    ajuste_emissao_onibus, ajuste_emissao_caminhao, &
    uso_carro, uso_moto, uso_onibus, uso_caminhao

    ! Declaração das variáveis utilizadas para efetuar os cálculos
    real :: emissao_poluente_carro, emissao_poluente_moto, emissao_poluente_onibus, &
    emissao_poluente_caminhao, emissao_total

    ! Declaração de namelist
    namelist /input/ n_carro, n_moto, n_onibus, n_caminhao, aumento_veiculos, &
    fator_emissao_carro, fator_emissao_moto, &
    fator_emissao_onibus, fator_emissao_caminhao, &
    ajuste_emissao_carro, ajuste_emissao_moto, &
    ajuste_emissao_onibus, ajuste_emissao_caminhao, &
    uso_carro, uso_moto, uso_onibus, uso_caminhao

    ! Abre o arquivo de namelist
    open(unit=10, file='dados_entrada.nml', status='old', action='read')

    ! Lê o arquivo de namelist e inicializa as variáveis
    read(10, nml=input)

    ! Fecha o arquivo de namelist
    close(10)

    ! Cálculos da emissão de cada veículo.
    ! OBS: o 'sum' é uma função para somar os elementos de um vetor
    emissao_poluente_carro = &
        n_carro * aumento_veiculos * uso_carro * sum(fator_emissao_carro) * ajuste_emissao_carro
    emissao_poluente_moto = &
        n_moto * aumento_veiculos * uso_moto * sum(fator_emissao_moto) * ajuste_emissao_moto
    emissao_poluente_onibus = &
        n_onibus * aumento_veiculos * uso_onibus * sum(fator_emissao_onibus) * ajuste_emissao_onibus
    emissao_poluente_caminhao = &
        n_caminhao * aumento_veiculos * uso_caminhao * sum(fator_emissao_caminhao) * ajuste_emissao_caminhao

    emissao_total = &
        emissao_poluente_carro + emissao_poluente_moto + emissao_poluente_onibus + emissao_poluente_caminhao

    ! Mostra o valor da emissão total na tela
    write(*, *) 'Emissao total dos veiculos (g/dia):'
    write(*, *) emissao_total
end program veiculos_emissao
