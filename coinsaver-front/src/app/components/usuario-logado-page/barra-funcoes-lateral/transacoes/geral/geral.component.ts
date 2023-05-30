import { Component } from '@angular/core';
import { ModalTrocarSenhaComponent } from '../../../modal-trocar-senha/modal-trocar-senha.component';
import { Router } from '@angular/router';
import { DialogService } from 'primeng/dynamicdialog';

@Component({
  selector: 'app-geral',
  templateUrl: './geral.component.html',
  styleUrls: ['./geral.component.css', '../../../usuario-logado-page.component.css']
})
export class GeralComponent {

  constructor(
    public router: Router,
    private dialogService: DialogService,
  ) { }

  retornaPaginaInicialUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

  abrirPerformancePage() {
    this.router.navigateByUrl('performance-page', {
      state: {
        data: {},
      },
    });
  }

  modalTrocarSenha() {
    this.dialogService.open(ModalTrocarSenhaComponent, {
      data: {},
      showHeader: false
    });
  }

}
