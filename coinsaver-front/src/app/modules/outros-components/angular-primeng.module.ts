import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { DynamicDialogModule, DialogService } from 'primeng/dynamicdialog';
import { DialogModule } from 'primeng/dialog';

const ANGULAR_PRIMENG_MODULES = [
  DynamicDialogModule,
  DialogModule,
]

@NgModule({
  declarations: [],
  imports: [
    CommonModule,
    ANGULAR_PRIMENG_MODULES,
  ],
  exports: [
    ANGULAR_PRIMENG_MODULES,
  ],
  providers: [
    DialogService,
  ]
})

export class AngularPrimeNgModule {}
