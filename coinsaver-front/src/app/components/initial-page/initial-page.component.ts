import { ModalCadastroComponent } from '../modal-cadastro/modal-cadastro.component';
import { Component } from '@angular/core';
import { DialogService } from 'primeng/dynamicdialog';

@Component({
  selector: 'app-initial-page',
  templateUrl: './initial-page.component.html',
  styleUrls: ['./initial-page.component.css']
})
export class InitialPageComponent {

  constructor(
    private dialogService: DialogService,
  ) {}

  showModalCadastro() {
    const page = this.dialogService.open(ModalCadastroComponent, {
      data: {},
      showHeader: false
    });
  }

}
