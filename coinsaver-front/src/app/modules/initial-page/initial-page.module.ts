import { HttpClientModule } from '@angular/common/http';
import { InitialPageRoutingModule } from './initial-page-routing.module';
import { InitialPageComponent } from './../../components/initial-page/initial-page.component';
import { NgModule } from '@angular/core';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { ModalCadastroComponent } from '../../components/modal-cadastro/modal-cadastro.component';
import { DynamicDialogModule, DialogService } from 'primeng/dynamicdialog';
import { DialogModule } from 'primeng/dialog';

@NgModule({
  declarations: [
    InitialPageComponent,
    ModalCadastroComponent,
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
