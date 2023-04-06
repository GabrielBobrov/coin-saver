import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-barra-funcoes-lateral',
  templateUrl: './barra-funcoes-lateral.component.html',
  styleUrls: ['./barra-funcoes-lateral.component.css']
})
export class BarraFuncoesLateralComponent {

  showFillerTransacoes = false;

  constructor(
    public router: Router,
  ) {}

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

  retornaPaginaInicialUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

}
