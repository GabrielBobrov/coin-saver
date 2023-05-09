import { Component } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { MessageService } from 'primeng/api';
import { DynamicDialogRef } from 'primeng/dynamicdialog';
import { AuthenticationRequestDto } from 'src/app/dtos/transactions/request/authentication.request.dto';
import { RegisterRequestDto } from 'src/app/dtos/transactions/request/register.request.dto';
import { AuthenticationService } from 'src/app/services/authentication/authentication.service';
import { DivisionsService } from 'src/app/services/divisions/divisions.service';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';

@Component({
  selector: 'app-cadastro-usuario-page',
  templateUrl: './cadastro-usuario-page.component.html',
  styleUrls: ['./cadastro-usuario-page.component.css']
})
export class CadastroUsuarioPageComponent {

  constructor(
    private autheticationService: AuthenticationService,
    private divisionService: DivisionsService,
    private transactionService: TransactionsService,
    public router: Router,
    public ref: DynamicDialogRef,
    private messageService: MessageService,
  ) { }

  generoControl = new FormControl();

  emailFormControl = new FormControl('', [Validators.required, Validators.email]);

  hideSenha = true;
  hideContraSenha = true;

  isContraEmailValido?: boolean;
  isContraPasswordValido?: boolean;

  authenticationRequestDto: AuthenticationRequestDto = {
    email: undefined,
    password: undefined,
  };

  registerRequestDto: RegisterRequestDto = {
    id: undefined,
    name: undefined,
    email: undefined,
    contraEmail: undefined,
    password: undefined,
    contraPassword: undefined,
  };

  createRegister(registerRequestDto: RegisterRequestDto) {

    console.log(registerRequestDto)

    console.log("email",this.isContraEmailValido)
    console.log("pass",this.isContraPasswordValido)

    if (!this.isContraPasswordValido && !this.isContraEmailValido) {

    }

    this.autheticationService.register(registerRequestDto).subscribe(
      (res) => {

        this.enviaTokenParaServices(res);

        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Usuário CRIADO com sucesso' });
        setTimeout(() => {
          this.cleanObjectRegisterRequestDto();
          this.fecharModal();
          this.retornaLoginPage();
        }, 1500);
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar CRIAR usuário' });
      }
    );
  }

  validaContraEmail(email: any, contraEmail: any) {
    if (email != contraEmail) {
      this.isContraEmailValido = false;
    } else {
      this.isContraEmailValido = true;
    }
  }

  validaContraPassword(password: any, contraPassword: any) {
    if (password != contraPassword) {
      this.isContraPasswordValido = false;
    } else {
      this.isContraPasswordValido = true;
    }
  }

  cleanObjectRegisterRequestDto() {
    this.registerRequestDto = {
      id: undefined,
      name: undefined,
      email: undefined,
      contraEmail: undefined,
      password: undefined,
      contraPassword: undefined,
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
  }

}
