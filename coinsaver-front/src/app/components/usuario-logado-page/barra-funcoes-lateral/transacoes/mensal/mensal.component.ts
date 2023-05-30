import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { DialogService } from 'primeng/dynamicdialog';
import { ModalTrocarSenhaComponent } from '../../../modal-trocar-senha/modal-trocar-senha.component';

@Component({
  selector: 'app-mensal',
  templateUrl: './mensal.component.html',
  styleUrls: ['./mensal.component.css', '../../../usuario-logado-page.component.css']
})
export class MensalComponent {

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

  retornaPaginaUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

}
