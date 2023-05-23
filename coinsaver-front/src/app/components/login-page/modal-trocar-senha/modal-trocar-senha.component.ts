import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { MessageService } from 'primeng/api';
import { DynamicDialogRef } from 'primeng/dynamicdialog';
import { AuthenticationRequestDto } from 'src/app/dtos/transactions/request/authentication.request.dto';
import { ChangePasswordRequestDto } from 'src/app/dtos/transactions/request/change-password.request.dto copy';
import { ClientsService } from 'src/app/services/clients/clients.service';

@Component({
  selector: 'app-modal-trocar-senha',
  templateUrl: './modal-trocar-senha.component.html',
  styleUrls: ['./modal-trocar-senha.component.css']
})
export class ModalTrocarSenhaComponent {

  hideSenhaAntiga = true;
  hideSenhaNova = true;
  hideSenhaConfirmacao = true;

  changePasswordnRequestDto: ChangePasswordRequestDto = {
    oldPassword: undefined,
    newPassword: undefined,
    newPasswordVerify: undefined,
  };

  constructor(
    private clientsService: ClientsService,
    private messageService: MessageService,
    public router: Router,
    public ref: DynamicDialogRef,
  ) {}

  trocarSenha(changePasswordnRequestDto: ChangePasswordRequestDto) {

    this.clientsService.changePassword(changePasswordnRequestDto).subscribe(
      (res) => {

        this.messageService.add({ severity: 'success', summary: 'Success', detail: 'Senha ALTERADA com sucesso.' });
        setTimeout(() => {
          this.cleanObjectChangePasswordRequestDto();
          this.fecharModal();
          this.returnLoginPage();
        }, 1500);
      },
      (error) => {
        this.messageService.add({ severity: 'error', summary: 'Error', detail: 'Erro ao tentar alterar senha.' });
      }
    );
  }

  cleanObjectChangePasswordRequestDto() {
    this.changePasswordnRequestDto = {
      oldPassword: undefined,
      newPassword: undefined,
      newPasswordVerify: undefined,
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
