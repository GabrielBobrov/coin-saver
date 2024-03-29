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
import { MAT_FORM_FIELD_DEFAULT_OPTIONS } from '@angular/material/form-field';
import { ModalUpdateTransacaoComponent } from '../../components/usuario-logado-page/barra-funcoes-lateral/transacoes/mensal/table-mensal/modal-update-transacao/modal-update-transacao.component';
import { DatepickerSelectionComponent } from 'src/app/shared/datepicker-selection/datepicker-selection.component';
import { ModalTrocarSenhaComponent } from '../../components/usuario-logado-page/modal-trocar-senha/modal-trocar-senha.component';
import { PerformancePageComponent } from '../../components/usuario-logado-page/performance-page/performance-page.component';
import { QuemSomosPageComponent } from '../../components/initial-page/quem-somos-page/quem-somos-page.component';
import { NossaHistoriaPageComponent } from '../../components/initial-page/nossa-historia-page/nossa-historia-page.component';
import { TecnologiasUtilizadasPageComponent } from 'src/app/components/initial-page/tecnologias-utilizadas-page/tecnologias-utilizadas-page.component';
import { ReferenciasPageComponent } from '../../components/initial-page/referencias-page/referencias-page.component';
import { EtapasPageComponent } from '../../components/initial-page/etapas-page/etapas-page.component';

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
    ModalUpdateTransacaoComponent,
    DatepickerSelectionComponent,
    ModalTrocarSenhaComponent,
    PerformancePageComponent,
    QuemSomosPageComponent,
    NossaHistoriaPageComponent,
    TecnologiasUtilizadasPageComponent,
    ReferenciasPageComponent,
    EtapasPageComponent
  ],
  imports: [
    InitialPageRoutingModule,
    HttpClientModule,
    AngularMaterialModule,
    AngularPrimeNgModule,
    CommonModule
  ],
  entryComponents: [

  ],
  bootstrap: [
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
    ModalUpdateTransacaoComponent,
    DatepickerSelectionComponent,
  ],
  providers: [
    {provide: MAT_FORM_FIELD_DEFAULT_OPTIONS, useValue: {appearence: 'fill'}},
  ]
})

export class InitialPageModule {}

platformBrowserDynamic()
  .bootstrapModule(InitialPageModule)
  .catch(erro => {});
