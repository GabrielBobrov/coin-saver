import { ModalCadastroUsuarioComponent } from '../modal-cadastro-usuario/modal-cadastro-usuario.component';
import { Component } from '@angular/core';
import { DialogService } from 'primeng/dynamicdialog';
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
    this.dialogService.open(ModalCadastroUsuarioComponent, {
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

  abrirQuemSomosPage() {
    this.router.navigateByUrl('quem-somos-page', {
      state: {
        data: {},
      },
    });
  }

  abrirNossaHistoriaPage() {
    this.router.navigateByUrl('nossa-historia-page', {
      state: {
        data: {},
      },
    });
  }

  abrirTecnologiasUtilizadasPage() {
    this.router.navigateByUrl('tecnologias-utilizadas-page', {
      state: {
        data: {},
      },
    });
  }

}
