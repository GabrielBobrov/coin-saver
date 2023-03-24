import { CadastroPageComponent } from './../../components/modal-cadastro/cadastro-page/cadastro-page.component';
import { InitialPageComponent } from './../../components/initial-page/initial-page.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    component: InitialPageComponent,
  },
  {
    path: 'cadastro-page',
    component: CadastroPageComponent
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class InitialPageRoutingModule { }
