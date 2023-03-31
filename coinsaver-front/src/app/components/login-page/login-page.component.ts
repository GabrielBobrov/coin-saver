import { ModalRedefinirSenhaComponent } from './modal-redefinir-senha/modal-redefinir-senha.component';
import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { DialogService } from 'primeng/dynamicdialog';
import { ErrorStateMatcher } from '@angular/material/core';
import { FormControl, FormGroupDirective, NgForm, Validators } from '@angular/forms';

export class MyErrorStateMatcher implements ErrorStateMatcher {
  isErrorState(control: FormControl | null, form: FormGroupDirective | NgForm | null): boolean {
    const isSubmitted = form && form.submitted;
    return !!(control && control.invalid && (control.dirty || control.touched || isSubmitted));
  }
}

@Component({
  selector: 'app-login-page',
  templateUrl: './login-page.component.html',
  styleUrls: ['./login-page.component.css']
})
export class LoginPageComponent {

  emailFormControl = new FormControl('', [Validators.required, Validators.email]);
  matcher = new MyErrorStateMatcher();

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
