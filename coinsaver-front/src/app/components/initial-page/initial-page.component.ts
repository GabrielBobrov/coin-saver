import { ModalCadastroUsuarioComponent } from '../modal-cadastro-usuario/modal-cadastro-usuario.component';
import { Component } from '@angular/core';
import { DialogService } from 'primeng/dynamicdialog';
import { Router } from '@angular/router';

@Component({
  selector: 'app-initial-page',
  templateUrl: './initial-page.component.html',
  styleUrls: ['./initial-page.component.css']
})
export class InitialPageComponent {

  constructor(
    private dialogService: DialogService,
    public router: Router,
  ) {}

  showModalCadastro() {
    this.dialogService.open(ModalCadastroUsuarioComponent, {
      data: {},
      showHeader: false
    });
  }

  abrirLoginPage() {
    this.router.navigateByUrl('login-page', {
      state: {
        data: {},
      },
    });
  }

}
