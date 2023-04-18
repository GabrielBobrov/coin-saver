import { AngularPrimeNgModule } from './../outros-components/angular-primeng.module';
import { AngularMaterialModule } from './../outros-components/angular-material.module';
import { HttpClientModule } from '@angular/common/http';
import { InitialPageRoutingModule } from './initial-page-routing.module';
import { InitialPageComponent } from './../../components/initial-page/initial-page.component';
import { NgModule } from '@angular/core';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { ModalCadastroUsuarioComponent } from '../../components/modal-cadastro-usuario/modal-cadastro-usuario.component';
import { CadastroUsuarioPageComponent } from '../../components/modal-cadastro-usuario/cadastro-usuario-page/cadastro-usuario-page.component';
import { LoginPageComponent } from '../../components/login-page/login-page.component';
import { ModalRedefinirSenhaComponent } from '../../components/login-page/modal-redefinir-senha/modal-redefinir-senha.component';
import { UsuarioLogadoPageComponent } from '../../components/usuario-logado-page/usuario-logado-page.component';
import { CommonModule } from '@angular/common';
import { BarraFuncoesLateralComponent } from '../../components/usuario-logado-page/barra-funcoes-lateral/barra-funcoes-lateral.component';
import { GeralComponent } from '../../components/usuario-logado-page/barra-funcoes-lateral/transacoes/geral/geral.component';
import { MensalComponent } from '../../components/usuario-logado-page/barra-funcoes-lateral/transacoes/mensal/mensal.component';
import { TableMensalComponent } from '../../components/usuario-logado-page/barra-funcoes-lateral/transacoes/mensal/table-mensal/table-mensal.component';
import { TableGeralComponent } from '../../components/usuario-logado-page/barra-funcoes-lateral/transacoes/geral/table-geral/table-geral.component';
import { ModalCadastroNovaTransacaoComponent } from '../../components/usuario-logado-page/barra-funcoes-lateral/transacoes/modal-cadastro-nova-transacao/modal-cadastro-nova-transacao.component';

@NgModule({
  declarations: [
    InitialPageComponent,
    ModalCadastroUsuarioComponent,
    CadastroUsuarioPageComponent,
    LoginPageComponent,
    ModalRedefinirSenhaComponent,
    UsuarioLogadoPageComponent,
    BarraFuncoesLateralComponent,
    GeralComponent,
    MensalComponent,
    TableMensalComponent,
    TableGeralComponent,
    ModalCadastroNovaTransacaoComponent,
  ],
  imports: [
    InitialPageRoutingModule,
    HttpClientModule,
    AngularMaterialModule,
    AngularPrimeNgModule,
    CommonModule
  ],
  providers: []
})

export class InitialPageModule {}

platformBrowserDynamic()
  .bootstrapModule(InitialPageModule)
  .catch(erro => {});
