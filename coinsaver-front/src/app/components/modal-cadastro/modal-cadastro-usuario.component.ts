import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { DynamicDialogRef } from 'primeng/dynamicdialog';

@Component({
  selector: 'app-modal-cadastro-usuario',
  templateUrl: './modal-cadastro-usuario.component.html',
  styleUrls: ['./modal-cadastro-usuario.component.css']
})
export class ModalCadastroUsuarioComponent {

  constructor(
    public router: Router,
    public ref: DynamicDialogRef
  ) {}

  abrirCadastroPage() {
    this.router.navigateByUrl('cadastro-usuario-page', {
      state: {
        data: {},
      },
    });
    this.ref.close();
  }

}
