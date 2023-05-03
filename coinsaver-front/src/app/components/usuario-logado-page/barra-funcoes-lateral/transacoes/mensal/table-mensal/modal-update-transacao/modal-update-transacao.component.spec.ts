import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ModalUpdateTransacaoComponent } from './modal-update-transacao.component';

describe('ModalUpdateTransacaoComponent', () => {
  let component: ModalUpdateTransacaoComponent;
  let fixture: ComponentFixture<ModalUpdateTransacaoComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ModalUpdateTransacaoComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ModalUpdateTransacaoComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
