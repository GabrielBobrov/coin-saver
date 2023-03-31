import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import {MatFormFieldModule} from '@angular/material/form-field';

const ANGULAR_MATERIAL_MODULES = [
  MatFormFieldModule,
]

@NgModule({
  declarations: [],
  imports: [
    CommonModule,
    ANGULAR_MATERIAL_MODULES,
  ],
  exports: [
    ANGULAR_MATERIAL_MODULES,
  ],
})

export class AngularMaterialModule {}
