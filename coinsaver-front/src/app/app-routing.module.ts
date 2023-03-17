import { AppComponent } from './app.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

const routes: Routes = [
  {
    path : "",
    component: AppComponent,
    children: [
      {
        path: "",
        loadChildren: () =>
          import("./modules/initial-page/initial-page.module").then(
            m => m.InitialPageModule
          )
      }
    ]
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes, {onSameUrlNavigation: 'reload'})],
  exports: [RouterModule]
})
export class AppRoutingModule { }
