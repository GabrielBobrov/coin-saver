import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { HttpClientModule } from '@angular/common/http';
import { DynamicDialogModule, DialogService } from 'primeng/dynamicdialog';
import { DialogModule } from 'primeng/dialog';
import { TableModule } from 'primeng/table';
import { TagModule } from 'primeng/tag';
import { ProgressBarModule } from 'primeng/progressbar';
import { ToastModule } from 'primeng/toast';
import { DropdownModule } from 'primeng/dropdown';
import { SliderModule } from 'primeng/slider';
import { MultiSelectModule } from 'primeng/multiselect';
import { AutoCompleteModule } from 'primeng/autocomplete';
import { InputTextModule } from 'primeng/inputtext';
import { TimelineModule } from 'primeng/timeline';
import { CardModule } from 'primeng/card';
import { ButtonModule } from 'primeng/button';



const ANGULAR_PRIMENG_MODULES = [
  DynamicDialogModule,
  DialogModule,
  TableModule,
  TagModule,
  ProgressBarModule,
  ToastModule,
  DropdownModule,
  SliderModule,
  MultiSelectModule,
  AutoCompleteModule,
  InputTextModule,
  ButtonModule,
  TimelineModule,
  CardModule
]

@NgModule({
  declarations: [],
  imports: [
    CommonModule,
    HttpClientModule,
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
