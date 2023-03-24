import { ModalCadastroComponent } from '../modal-cadastro/modal-cadastro.component';
import { Component } from '@angular/core';
import { DialogService, DynamicDialogRef } from 'primeng/dynamicdialog';
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
    this.dialogService.open(ModalCadastroComponent, {
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
