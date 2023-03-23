import { CadastroPageComponent } from './../cadastro-page/cadastro-page.component';
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

  showModalCadastroPage() {
    const page = this.dialogService.open(CadastroPageComponent, {
      data: {},
      width: 'auto',
    });
  }

}
