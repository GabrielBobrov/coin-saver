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
    if (registerRequestDto.name != undefined) {
      this.validaContraEmail(registerRequestDto.email, registerRequestDto.contraEmail);
      if (this.isContraEmailValido) {
        this.validaContraPassword(registerRequestDto.password, registerRequestDto.contraPassword);
        if (this.isContraPasswordValido) {
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
              if (error.error.detail) {
                this.messageService.add({ severity: 'error', summary: 'Error', detail: error.error.detail });
              }
            }
          );
        }
      }
    } else {
      this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar CRIAR usuário. Preencha todos os campos.' });
    }
  }

  validaContraEmail(email: any, contraEmail: any) {
    if (email != undefined || contraEmail != undefined) {
      if (email != contraEmail) {
        this.isContraEmailValido = false;
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'O EMAIL digitado não confere com a confirmação.' });
      } else {
        this.isContraEmailValido = true;
      }
    } else {
      this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar CRIAR usuário. Preencha todos os campos.' });
    }
  }

  validaContraPassword(password: any, contraPassword: any) {
    if (password != undefined || contraPassword != undefined) {
      if (password != contraPassword) {
        this.isContraPasswordValido = false;
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'A SENHA digitada não confere com a confirmação.' });
      } else {
        this.isContraPasswordValido = true;
      }
    } else {
      this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar CRIAR usuário. Preencha todos os campos.' });
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
