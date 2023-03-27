import { ModalRedefinirSenhaComponent } from './modal-redefinir-senha/modal-redefinir-senha.component';
import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { DialogService } from 'primeng/dynamicdialog';

@Component({
  selector: 'app-login-page',
  templateUrl: './login-page.component.html',
  styleUrls: ['./login-page.component.css']
})
export class LoginPageComponent {

  constructor(
    public router: Router,
    private dialogService: DialogService,
  ) {}

  abrirCadastroPage() {
    this.router.navigateByUrl('cadastro-page', {
      state: {
        data: {},
      },
    });
  }

  showModalRedefinirSenha() {
    this.dialogService.open(ModalRedefinirSenhaComponent, {
      data: {},
      showHeader: false
    });
  }

  abrirUsuarioLogadoPage() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

}
