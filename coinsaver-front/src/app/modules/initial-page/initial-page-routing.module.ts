import { UsuarioLogadoPageComponent } from './../../components/usuario-logado-page/usuario-logado-page.component';
import { LoginPageComponent } from './../../components/login-page/login-page.component';
import { CadastroUsuarioPageComponent } from '../../components/modal-cadastro-usuario/cadastro-usuario-page/cadastro-usuario-page.component';
import { InitialPageComponent } from './../../components/initial-page/initial-page.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { GeralComponent } from 'src/app/components/usuario-logado-page/barra-funcoes-lateral/transacoes/geral/geral.component';
import { MensalComponent } from 'src/app/components/usuario-logado-page/barra-funcoes-lateral/transacoes/mensal/mensal.component';
import { PerformancePageComponent } from 'src/app/components/usuario-logado-page/performance-page/performance-page.component';
import { QuemSomosPageComponent } from 'src/app/components/initial-page/quem-somos-page/quem-somos-page.component';

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    component: InitialPageComponent,
  },
  {
    path: 'quem-somos-page',
    component: QuemSomosPageComponent
  },
  {
    path: 'login-page',
    component: LoginPageComponent
  },
  {
    path: 'cadastro-usuario-page',
    component: CadastroUsuarioPageComponent
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
  },
  {
    path: 'performance-page',
    component: PerformancePageComponent
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class InitialPageRoutingModule { }
