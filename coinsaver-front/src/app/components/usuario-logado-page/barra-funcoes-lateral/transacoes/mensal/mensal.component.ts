import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-mensal',
  templateUrl: './mensal.component.html',
  styleUrls: ['./mensal.component.css', '../../../usuario-logado-page.component.css']
})
export class MensalComponent {

  constructor(
    public router: Router,
  ) { }

  retornaPaginaUsuarioLogado() {
    this.router.navigateByUrl('usuario-logado-page', {
      state: {
        data: {},
      },
    });
  }

}
