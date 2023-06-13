import { ModalRedefinirSenhaComponent } from './modal-redefinir-senha/modal-redefinir-senha.component';
import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { DialogService, DynamicDialogRef } from 'primeng/dynamicdialog';
import { FormControl, Validators } from '@angular/forms';
import { AuthenticationRequestDto } from 'src/app/dtos/transactions/request/authentication.request.dto';
import { MessageService } from 'primeng/api';
import { AuthenticationService } from 'src/app/services/authentication/authentication.service';
import { DivisionsService } from 'src/app/services/divisions/divisions.service';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';
import { ClientsService } from 'src/app/services/clients/clients.service';

@Component({
  selector: 'app-login-page',
  templateUrl: './login-page.component.html',
  styleUrls: ['./login-page.component.css']
})
export class LoginPageComponent {

  constructor(
    private autheticationService: AuthenticationService,
    private divisionService: DivisionsService,
    private transactionService: TransactionsService,
    private clientsService: ClientsService,
    public router: Router,
    public ref: DynamicDialogRef,
    private messageService: MessageService,
    private dialogService: DialogService,
  ) {}

  emailFormControl = new FormControl('', [Validators.required, Validators.email]);

  hideSenha = true;

  authenticationRequestDto: AuthenticationRequestDto = {
    email: undefined,
    password: undefined,
  };

  showModalRedefinirSenha() {
    this.dialogService.open(ModalRedefinirSenhaComponent, {
      data: {},
      showHeader: false
    });
  }

  fazerLoginUsuarioPage(authenticationRequestDto: AuthenticationRequestDto) {
    if (authenticationRequestDto.email == undefined || authenticationRequestDto.password == undefined ||
      authenticationRequestDto.email == '' || authenticationRequestDto.password == '') {
      this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar LOGAR usuário. Preencha todos os campos.' });
    } else {
      this.autheticationService.authenticate(authenticationRequestDto).subscribe(
        (res) => {
          this.enviaTokenParaServices(res);
          this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Usuário LOGADO com sucesso' });
          setTimeout(() => {
            this.cleanObjectAuthenticationRequestDto();
            this.fecharModal();
            this.retornaLoginPage();
          }, 1500);
        },
        (error) => {
          console.log(error.error)
          if (error.error.status == 500) {
            this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar LOGAR usuário. Email ou senha incorretos.' });
          } else {
            this.messageService.add({ severity: 'error', summary: 'Error', detail: error.error.detail });
          }
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

  retornaLoginPage() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

  enviaTokenParaServices(token: any) {
    this.autheticationService.recebeToken(token);
    this.divisionService.recebeToken(token);
    this.transactionService.recebeToken(token);
    this.clientsService.recebeToken(token);
  }

}
