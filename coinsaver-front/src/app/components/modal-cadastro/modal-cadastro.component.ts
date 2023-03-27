import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { DynamicDialogRef } from 'primeng/dynamicdialog';

@Component({
  selector: 'app-modal-cadastro',
  templateUrl: './modal-cadastro.component.html',
  styleUrls: ['./modal-cadastro.component.css']
})
export class ModalCadastroComponent {

  constructor(
    public router: Router,
    public ref: DynamicDialogRef
  ) {}

  abrirCadastroPage() {
    this.router.navigateByUrl('cadastro-page', {
      state: {
        data: {},
      },
    });
    this.ref.close();
  }

}
