import { HttpClientModule } from '@angular/common/http';
import { InitialPageRoutingModule } from './initial-page-routing.module';
import { InitialPageComponent } from './../../components/initial-page/initial-page.component';
import { NgModule } from '@angular/core';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { ModalCadastroComponent } from '../../components/modal-cadastro/modal-cadastro.component';
import { DynamicDialogModule, DialogService } from 'primeng/dynamicdialog';
import { DialogModule } from 'primeng/dialog';
import { CadastroPageComponent } from '../../components/modal-cadastro/cadastro-page/cadastro-page.component';
import { LoginPageComponent } from '../../components/login-page/login-page.component';
import { ModalRedefinirSenhaComponent } from '../../components/login-page/modal-redefinir-senha/modal-redefinir-senha.component';
import { UsuarioLogadoPageComponent } from '../../components/usuario-logado-page/usuario-logado-page.component';

@NgModule({
  declarations: [
    InitialPageComponent,
    ModalCadastroComponent,
    CadastroPageComponent,
    LoginPageComponent,
    ModalRedefinirSenhaComponent,
    UsuarioLogadoPageComponent,
  ],
  imports: [
    InitialPageRoutingModule,
    HttpClientModule,
    DynamicDialogModule,
    DialogModule
  ],
  providers: [
    DialogService,
  ]
})

export class InitialPageModule {}

platformBrowserDynamic()
  .bootstrapModule(InitialPageModule)
  .catch(erro => {});
