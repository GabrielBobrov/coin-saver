import { Component } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { MessageService } from 'primeng/api';
import { DynamicDialogRef } from 'primeng/dynamicdialog';
import { AuthenticationRequestDto } from 'src/app/dtos/transactions/request/authentication.request.dto';
import { ClientsService } from 'src/app/services/clients/clients.service';

@Component({
  selector: 'app-modal-redefinir-senha',
  templateUrl: './modal-redefinir-senha.component.html',
  styleUrls: ['./modal-redefinir-senha.component.css']
})
export class ModalRedefinirSenhaComponent {

  constructor(
    private clientsService: ClientsService,
    private messageService: MessageService,
    public router: Router,
    public ref: DynamicDialogRef,
  ) {}

  emailFormControl = new FormControl('', [Validators.required, Validators.email]);

  email: string | undefined;

  isRedefinirSenha: boolean = false;

  authenticationRequestDto: AuthenticationRequestDto = {
    email: undefined,
    password: undefined,
  };

  redefinirSenha(authenticationRequestDto: AuthenticationRequestDto) {
    this.email = authenticationRequestDto.email;
    if (this.email == undefined || this.email == '') {
      this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar redefinir senha. Verifique o email digitado.' });
    } else {
      this.clientsService.recoverPassword(this.email).subscribe(
        (res) => {
          this.messageService.add({ severity: 'success', summary: 'Success', detail: 'EMAIL enviado com sucesso.' });
          this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Verifique sua caixa de entrada e span, faça login e troque sua senha antiga.' });
          setTimeout(() => {
            this.cleanObjectAuthenticationRequestDto();
            this.fecharModal();
            this.returnLoginPage();
          }, 5000);
        },
        (error) => {
          this.messageService.add({ severity: 'error', summary: 'Error', detail: error.error.detail });
        }
      );
    }
  }

  cleanObjectAuthenticationRequestDto() {
    this.authenticationRequestDto = {
      email: undefined,
      password: undefined,
    }
  }

  fecharModal() {
    this.ref.close();
  }

  returnLoginPage() {
    this.router.navigateByUrl('login-page', {
      state: {
        data: {},
      },
    });
  }
}
