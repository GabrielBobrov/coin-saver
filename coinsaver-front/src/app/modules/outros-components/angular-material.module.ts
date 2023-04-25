import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { HttpClientModule } from '@angular/common/http';
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatTableModule } from '@angular/material/table';
import { MatNativeDateModule } from '@angular/material/core';

const ANGULAR_MATERIAL_MODULES = [
  MatFormFieldModule,
  FormsModule,
  ReactiveFormsModule,
  MatSelectModule,
  MatInputModule,
  MatIconModule,
  MatSidenavModule,
  MatTableModule,
  MatNativeDateModule
]

@NgModule({
  declarations: [],
  imports: [
    CommonModule,
    HttpClientModule,
    ANGULAR_MATERIAL_MODULES,
  ],
  exports: [
    ANGULAR_MATERIAL_MODULES,
  ],
})

export class AngularMaterialModule {}
