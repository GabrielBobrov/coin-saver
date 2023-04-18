import { Component } from '@angular/core';
import { FormControl } from '@angular/forms';

@Component({
  selector: 'app-modal-cadastro-nova-transacao',
  templateUrl: './modal-cadastro-nova-transacao.component.html',
  styleUrls: ['./modal-cadastro-nova-transacao.component.css']
})
export class ModalCadastroNovaTransacaoComponent {

  transactionCategoryTypeControl = new FormControl();
  categoryTypeControl = new FormControl();
  transactionTypeControl = new FormControl();

}
