import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { DialogService } from 'primeng/dynamicdialog';
import { ModalCadastroNovaTransacaoComponent } from './transacoes/modal-cadastro-nova-transacao/modal-cadastro-nova-transacao.component';

@Component({
  selector: 'app-barra-funcoes-lateral',
  templateUrl: './barra-funcoes-lateral.component.html',
  styleUrls: ['./barra-funcoes-lateral.component.css']
})
export class BarraFuncoesLateralComponent {

  showFillerTransacoes = false;
  showFillerNovaTransacao = false;

  isOpen = false;

  constructor(
    public router: Router,
    private dialogService: DialogService,
  ) {}

  abrirMenuLateral() {
    this.isOpen = true;
  }

  fecharMenuLateral() {
    this.isOpen = false;
  }

  abrirTransacoesGeral() {
    this.router.navigateByUrl('transacoes-gerais-page', {
      state: {
        data: {},
      },
    });
  }

  abrirTransacoesMensal() {
    this.router.navigateByUrl('transacoes-mensais-page', {
      state: {
        data: {},
      },
    });
  }

  abrirModalCadastroNovaTransacao() {
    this.dialogService.open(ModalCadastroNovaTransacaoComponent, {
      data: {},
      showHeader: false
    });

  }

  retornaPaginaInicialUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

  sairLoginUsuario() {
    this.router.navigateByUrl('', {
      state: {
        data: {},
      },
    });
  }

}
