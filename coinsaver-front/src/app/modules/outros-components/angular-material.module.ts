import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatInputModule } from '@angular/material/input';

const ANGULAR_MATERIAL_MODULES = [
  MatFormFieldModule,
  FormsModule,
  ReactiveFormsModule,
  MatSelectModule,
  MatInputModule,
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
