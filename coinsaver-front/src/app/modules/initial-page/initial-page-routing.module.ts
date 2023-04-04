import { UsuarioLogadoPageComponent } from './../../components/usuario-logado-page/usuario-logado-page.component';
import { LoginPageComponent } from './../../components/login-page/login-page.component';
import { CadastroPageComponent } from './../../components/modal-cadastro/cadastro-page/cadastro-page.component';
import { InitialPageComponent } from './../../components/initial-page/initial-page.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { GeralComponent } from 'src/app/components/usuario-logado-page/barra-funcoes-lateral/transacoes/geral/geral.component';
import { MensalComponent } from 'src/app/components/usuario-logado-page/barra-funcoes-lateral/transacoes/mensal/mensal.component';

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    component: InitialPageComponent,
  },
  {
    path: 'login-page',
    component: LoginPageComponent
  },
  {
    path: 'cadastro-page',
    component: CadastroPageComponent
  },
  {
    path: 'usuario-logado-page',
    component: UsuarioLogadoPageComponent
  },
  {
    path: 'transacoes-gerais-page',
    component: GeralComponent
  },
  {
    path: 'transacoes-mensais-page',
    component: MensalComponent
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class InitialPageRoutingModule { }
